//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Michael E. Cotterell
 *  @version 1.0
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes UCFOpt.scala Optimize.scala
 *  @run     scala -cp ../../classes:classes process.UCFSim
 *  @run     scala -cp ../../classes:classes process.UCFSim2 nTN nRN nMD nNP nAC
 *  @run     scala -cp ../../classes:classes process.UCFTheory
 *  @run     scala -cp ../../classes:classes process.UCFOpt
 */

package process

import collection.mutable.{ListBuffer, ListMap, HashMap}

import scalation.dsl._
import scalation.process._
import scalation.math.Matrices.MatrixD
import scalation.math.Vectors.{VectorD, VectorI}
import scalation.queueingnet.JacksonNet
import scalation.random.{Random, Uniform, Variate, Exponential}
import scalation.stat.Statistic
import scalation.util.Monitor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait provides the rates and number of arrivals for the UCF Model
 */
trait UCFParams
{

    val λ = new VectorD ( 4.,  2.,  0.,  0.,  0.)
    val μ = new VectorD (15.,  10., 3.,  7., 10.)

    var (nArrivalsFrontDoor, iArrivalFRV) = (200, Exponential(1./λ(0)))
    var (nArrivalsAmbulance, iArrivalARV) = (100, Exponential(1./λ(1)))

    val μTN = Exponential(1./μ(0))
    val μRN = Exponential(1./μ(1))
    val μMD = Exponential(1./μ(2))
    val μNP = Exponential(1./μ(3))
    val μAC = Exponential(1./μ(4))

    var Δ = Uniform (0, 0)

    var endTime = 100.

} // UCFParams

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class defines a simple process-interaction model of an Urgent Care
 *  Facility (UCF) model where service is provided by different kinds of nurses,
 *  nurse practitionaers, and administrative clerks.A patient will first see a
 *  Triage Nurse and then a Registered Nurse.
 *  @param x  the input vector specifying the number of servers at each node.
 */
class UCFModel (x: VectorI, animate: Boolean = false) extends Model ("UCFModel", animate) with UCFParams
{
    Monitor.traceOff ()

    val nTN           = x(0) // Triage Nurses
    val nRN           = x(1) // Registered Nurses
    val nMD           = x(2) // Medical Doctors
    val nNP           = x(3) // Nurse Practition
    val nAC           = x(4) // Administrative Clerks          

    val frontDoor     = new Source ("frontDoor", this, WalkInPatient, nArrivalsFrontDoor, iArrivalFRV, (10., 180.))
    val ambulance     = new Source ("ambulance", this, AmbulancePatient, nArrivalsAmbulance, iArrivalARV, (10., 85.))
 
    val tnQ           = new WaitQueue ("tnQ", (120., 185.))
    val rnQ           = new WaitQueue ("rnQ", (300., 185.))
    val mdQ           = new WaitQueue ("mdQ", (400.,  85.))
    val npQ           = new WaitQueue ("npQ", (400., 285.))
    val acQ           = new WaitQueue ("acQ", (500., 185.))

    val tn            = new Resource ("tn", tnQ, nTN, μTN, (170., 180.))
    val rn            = new Resource ("rn", rnQ, nRN, μRN, (350., 180.))
    val md            = new Resource ("md", mdQ, nMD, μMD, (450.,  80.))
    val np            = new Resource ("np", npQ, nNP, μNP, (450., 280.))
    val ac            = new Resource ("ac", acQ, nAC, μAC, (550., 180.))

    val door          = new Sink ("door", (650., 180.))

    val frontDoor2tnQ = new Transport ("2tnQ", Δ, frontDoor, tnQ)
    val ambulance2rnQ = new Transport ("2rnQ", Δ, ambulance, rnQ)
    val tn2rnQ        = new Transport ("tn2rnQ", Δ, tn, rnQ)
    val rn2mdQ        = new Transport ("rn2mdQ", Δ, rn, mdQ)
    val rn2npQ        = new Transport ("rn2npQ", Δ, rn, npQ)
    val md2acQ        = new Transport ("md2acQ", Δ, md, acQ)
    val np2acQ        = new Transport ("np2acQ", Δ, np, acQ)
    val ac2door       = new Transport ("ac2door", Δ, ac, door)

    val r = Random ()

    addComponents (List (frontDoor, 
			 ambulance, 
			 tnQ, tn, 
			 rnQ, rn, 
			 mdQ, md, 
			 npQ, np, 
			 acQ, ac, 
			 door,
                         frontDoor2tnQ, ambulance2rnQ,
                         tn2rnQ, rn2mdQ, rn2npQ, md2acQ, 
                         np2acQ, ac2door))

    abstract class Patient (pType: String) extends SimActor ("p-%s".format(pType), this)


    case class AmbulancePatient () extends Patient ("ambulance")
    {
        def act () 
        {
            ambulance2rnQ.move ()
	    if (rn.busy) rnQ.waitIn else rnQ.noWait
            rn.utilize
	    rn.release
	    if (r.gen <= 0.25) {
	        rn2mdQ.move
		if (md.busy) mdQ.waitIn else mdQ.noWait
                md.utilize
	        md.release
                md2acQ.move
	    } else {
	        rn2npQ.move
	        if (np.busy) npQ.waitIn else npQ.noWait
	        np.utilize
	        np.release
	        np2acQ.move
	    }
	    if (ac.busy) acQ.waitIn else acQ.noWait
	    ac.utilize
	    ac.release
	    ac2door.move
	    door.leave
        } // act
    } // AmbulancePatient
    
    case class WalkInPatient () extends Patient ("walkin")
    {
        def act () 
        {
            frontDoor2tnQ.move
            if (tn.busy) tnQ.waitIn else tnQ.noWait
	    tn.utilize
	    tn.release
            tn2rnQ.move
	    if (rn.busy) rnQ.waitIn else rnQ.noWait
	    rn.utilize
	    rn.release
	    if (r.gen <= 0.25) {
	        rn2mdQ.move
		if (md.busy) mdQ.waitIn else mdQ.noWait
                md.utilize
	        md.release
                md2acQ.move
	    } else {
	        rn2npQ.move
	        if (np.busy) npQ.waitIn else npQ.noWait
	        np.utilize
	        np.release
	        np2acQ.move
	    }
	    if (ac.busy) acQ.waitIn else acQ.noWait
	    ac.utilize
	    ac.release
	    ac2door.move
	    door.leave
        } // act
    } // WalkInPatient

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the simulation (includes scheduling all Sources) returning summary
     *  statistics.
     *  @param startTime the time at which the simulation is to begin
     */
    override def simulate (startTime: Double = 0.): ListBuffer [Statistic] =
    {
        _clock = startTime
        // trace (this, "starts", this, _clock)
        for (p <- parts) {
            // trace (this, "establish x = " + p.at(0) + " y = " + p.at(1), p, _clock)
            p.setDirector (this)
            if (p.isInstanceOf [Source]) reschedule (p.asInstanceOf [Source]) 
        } // for

        simulating = true
        start ()                              // start the director thread/actor
        val future = this !! RETURN_RESULTS   // results returned in a future reply
        val results = future ()
        println ("<<<<<<<<<<<<<<<<<<<<<<< future returned - simulation finished >>>>>>>>>>>>>>>>>>>>>>")
        report
        getStatistics
    } // simulate (override)

} // UCFModel class

object UCFSim extends App with UCFParams
{
    //Δ           = Uniform (900, 1100)
    endTime     = 100.
    val x       = new VectorI (1, 1, 1, 1, 1)
    val ucfm    = new UCFModel (x, false)
    val results = ucfm.simulate (0.)
} // UCFSim

object UCFSim2 extends App with UCFParams
{
    val x       = new VectorI (args(0).toInt, args(1).toInt, args(2).toInt, args(3).toInt, args(4).toInt)
    val ucfm    = new UCFModel (x)
    val results = ucfm.simulate (0.)
} // UCFSim2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This is an example of optimzation of the UCF model using IntegerLocalSearch
 *  and a cost function. The only constraints we have on the input vector are
 *  that each element must be positive.
 */
object UCFOpt extends App with UCFParams with SimOptDSL
{

    val PENALTY        = 1.E8 
    //val cost           = new VectorD (100., 200., 50.)
    val prcost         = new VectorD ( 20., 30., 120., 75. , 7.75 )
    val C              = new VectorD ( 30., 35., 600., 250., 15.)  
    var ucfm: UCFModel = null

    def f (x: VectorI): Double = 
    {
        val (nTN, nRN, nMD, nNP, nAC) = (x(0), x(1), x(2), x(3), x(4))

        // Penalty for numbers that don't make sense (i.e., < 1)
	for (i <- 0 until x.dim) if (x(i) < 1) return -PENALTY * (1 - x(i))

        //for (i <- 0 until x.dim) if (x(i) > 10) return -PENALTY * x(i)

        // Impose maximums on certain parts of the input vector
        //if (nTN > 2) return PENALTY * (-2 + nTN)
	//if (nRN > 5) return PENALTY * (-5 + nRN)
        //if (nMD > 2) return PENALTY * (-2 + nMD)
	//if (nNP > 4) return PENALTY * (-4 + nNP)
        //if (nAC > 5) return PENALTY * (-5 + nAC)

        ucfm = new UCFModel (x)

        val results  = ucfm.simulate (0.)
        //val waitTime = results(2).mean + results(4).mean + results(6).mean + results(8).mean + results(10).mean
        val wQ       = new VectorD (results(2).mean, results(4).mean, results(6).mean, results(8).mean, results(10).mean)
        val workers  = new VectorD (nTN, nRN, nMD, nNP, nAC)

	def r (x: VectorI) = 400. * results(8).num + 750. * results(6).num
        
        def c (x: VectorI) = ((prcost dot workers) + (C dot wQ)) * ucfm.clock

        def p (x: VectorI) = (r(x) - c(x)) / 24.

        println ("---------------------------------------------------------------")
        println ("simulated an UCF with %2d TN, %2d RN, %2d MD, %2d NP, %2d AC, r = %8.2f, c = %8.2f, p = %8.2f, clock = %4.3f".format(nTN, nRN, nMD, nNP, nAC, r(x), c(x), p(x), ucfm.clock))
        println ("---------------------------------------------------------------")

        p(x)
    } // f

    val x0 = new VectorI (5); x0.set (1)

/*
    /** Method of batch means to determine batch size n and number of batches b
     */
    var n       = 5
    var b       = 5

    val maxCor  = .2
    val maxCIH  = .2
    
    var testResult: ListBuffer [Statistic] = null

    do {
        n *= 2
        nArrivalsFrontDoor = ((2. * n * b) / 3.).toInt
        nArrivalsAmbulance = ((1. * n * b) / 3.).toInt
        val test           = new UCFModel (x0, false)
        testResult         = test.simulate (0.)
    } while (testResult(6).autoCor(b) >= maxCor && testResult(8).autoCor(b) >= maxCor)

    println("optimal batch size is %s".format(n))

    do {
        b *= 2
        nArrivalsFrontDoor = ((2. * n * b) / 3.).toInt
        nArrivalsAmbulance = ((1. * n * b) / 3.).toInt
        val test           = new UCFModel (x0, false)
        testResult         = test.simulate (0.)
    } while (testResult(6).autoCor >= maxCor && testResult(8).autoCor >= maxCor)
*/


    /** Reset the number of arrivals
     */
    nArrivalsFrontDoor = 200
    nArrivalsAmbulance = 100

    val optimizer = new IntegerLocalOptimizer
    //val optimizer = new IntegerTabuOptimizer
    //val optimizer = new GeneticAlgorithmOptimizer
 
    val objfunc = f _ using optimizer
    val result = max (objfunc) (x0, .1)

    println ("###############################################################")
    println ("optimal solution x = " + result)
    println ("###############################################################")
    println ("optimizer iterations = " + objfunc.iters )
    println ("total number of simulations = " + objfunc.calls )
    println ("max batch size = " + objfunc.batchSize )
    println ("###############################################################")

} // UCFOpt

object UCFTheory extends App with UCFParams
{
    //                            (DESTINATIONS)
    //                            TN    RN    MD    NP    AC
    val p  = new MatrixD ((5, 5), 0.00, 1.00, 0.00, 0.00, 0.00, // TN
                                  0.00, 0.00, 0.25, 0.75, 0.00, // RN
                                  0.00, 0.00, 0.00, 0.00, 1.00, // MD (SOURCES)
                                  0.00, 0.00, 0.00, 0.00, 1.00, // NP
                                  0.00, 0.00, 0.00, 0.00, 0.00) // AC
    val jqn = new JacksonNet (p, λ, μ)
    jqn.check
    jqn.report

} // UCFTheory
