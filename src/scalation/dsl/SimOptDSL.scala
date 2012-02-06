//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Michael Cotterell
 *  @version 1.0
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../scalation/classes -d ../../classes SimOptDSL.scala
 *  @run     scala -cp ../../scalation/classes:../../classes scalation.dsl.SimOptDSLTest
 */

package scalation
package dsl

import collection.mutable.{ListBuffer, ListMap, HashMap}

import scalation.math.Vectors.{VectorD, VectorI}
import scalation.math.VectorN
import scalation.maxima.{ConjGradient, GeneticAlgorithm, IntegerLocalSearch, IntegerTabuSearch}
import scalation.stat.StatVector

/** Optimizer
 */
trait Optimizer [Input, Output]
{

    /** Sets the objective function for this optimizer.
     */
    def setObjective [OptType <: Optimizer[Input, Output], MinMax] 
        (f: ObjFunc [Input, Output, OptType, MinMax]): Optimizer [Input, Output]

    /** Solves the optimization problem.
     */
    def solve (x: Input): (Input, Output)

} // Optimizer

trait MinimaOptimization
trait MaximaOptimization
trait UnreadyOptimization

trait IntegerOptimizer extends Optimizer [VectorI, Double]
trait RealOptimizer extends Optimizer [VectorD, Double]

/** IntegerLocalOptimizer
 *  This class is a proxy for Dr. Miller's Integer Local Search
 */
case class IntegerLocalOptimizer (objf: Option[VectorI => Double] = None) 
    extends IntegerOptimizer with MaximaOptimization 
{
    def setObjective [OptType <: Optimizer[VectorI, Double], MinMax] (f: ObjFunc [VectorI, Double, OptType, MinMax]) 
        = new IntegerLocalOptimizer (Some(f))

    def solve (x0: VectorI): (VectorI, Double) = objf match {
        case Some(f) => (new IntegerLocalSearch (f)).solve(x0)
        case _       => (x0, 0.)
    } // solve

} // IntegerLocalOptimizer

/** IntegerTabuOptimizer
 *  This class is a proxy for Dr. Miller's Integer Tabu Search
 */
case class IntegerTabuOptimizer (objf: Option[VectorI => Double] = None) 
    extends IntegerOptimizer with MaximaOptimization 
{
    def setObjective [OptType <: Optimizer[VectorI, Double], MinMax] (f: ObjFunc [VectorI, Double, OptType, MinMax]) 
        = new IntegerTabuOptimizer (Some(f))

    def solve (x0: VectorI): (VectorI, Double) = objf match {
        case Some(f) => (new IntegerTabuSearch (f)).solve(x0)
        case _       => (x0, 0.)
    } // solve

} // IntegerTabuOptimizer

/** GeneticAlgorithmOpt
 *  This class is a proxy for Dr. Miller's Integer Tabu Search
 */
case class GeneticAlgorithmOptimizer (objf: Option[VectorI => Double] = None) 
    extends IntegerOptimizer with MaximaOptimization 
{
    def setObjective [OptType <: Optimizer[VectorI, Double], MinMax] (f: ObjFunc [VectorI, Double, OptType, MinMax]) 
        = new GeneticAlgorithmOptimizer (Some(f))

    def solve (x0: VectorI): (VectorI, Double) = objf match {
        case Some(f) => (new GeneticAlgorithm (f, x0)).solve
        case _       => (x0, 0.)
    } // solve

} // GeneticAlgorithmOptimizer

/** Objective Function
 */
class ObjFunc [Input, Output, OptType <: Optimizer[Input, Output], MinMax]
              (val f: Input => Output, val optimizer: Option[OptType] = None, var vMax: Double = .5)
              (implicit val enableCache: Boolean, implicit val nuo: Numeric[Output])
    extends Function1 [Input, Output]
{
    // This is the simple cache for the objectie function
    private val cache = ListMap.empty [Input, Output]    

    var batchSize = 0
    var iters     = 0
    var calls     = 0

/*    def evaluate (x: Input) =
    {

        // initial batch size
        var b               = 10

        var svp: StatVector = new StatVector (b)
        var sv:  StatVector = null
	var bm:  StatVector = null
        var bmp: StatVector = new StatVector (1)

        // initial batch
        calls += svp.dim
        for (i <- 0 until svp.dim) {
	    svp(i) = nuo.toDouble(f(x))
            println("\t\tcreating initial batch (replication %4d / %4d)".format(i+1, svp.dim))
	    println("\t\titers = %d".format(iters))
	} // for

        val corr = 0.1

	// increase the batch size to that it's uncorrelated
        /*while (svp.autocorrelation > corr && svp.dim < 400) {
            //println("\t\tdata too correlated (%.4f > %.4f)".format(svp.autocorrelation, corr))
            b = svp.dim + 1
            println("\t\tincreasing batch initial batch size to %d".format(b))
            println("\t\titers = %d".format(iters))
            sv = new StatVector (b)
            for (i <- 0 until svp.dim) sv(i) = svp(i)
            calls += 1
            sv(svp.dim) = nuo.toDouble(f(x))
            svp = sv
	} // while
*/
        // set the initial batch mean
        bmp(0) = svp.mean

        // double the batches until the variance is less than vMax
        do {

            bm = if (bmp.dim == 1) new StatVector (bmp.dim * 10) else new StatVector (bmp.dim + 2)
            for (i <- 0 until bmp.dim) bm(i) = bmp(i)

	    for (n <- bmp.dim until bm.dim) {
	        sv = new StatVector (b)
                calls += sv.dim
       	        for (i <- 0 until sv.dim) {
                    sv(i) = nuo.toDouble(f(x))
                    println("\t\tbatch means variance = %f > %f ".format(bmp.variance / bmp.dim, vMax * bmp.mean))
		    println("\t\t(batch %4d / %4d) (replication %4d / %4d)".format(n+1, bm.dim, i+1, sv.dim))
		    println("\t\titers = %d".format(iters))
                }
                bm(n) = sv.mean
            } // for

            bmp = bm            

            println("batch means = " + bm)
	    println("\t\tbatch means variance = " + (bm.variance / bm.dim))

	} while ((bmp.variance / bmp.dim) > (vMax * bmp.mean) && bmp.dim < 30)

	println("***************************************")
        println("***************************************")
        println("***************************************")
        
        println("Evaluated with input = " + x)
        println("sufficiently uncorrelated batch size = " + b)
        println("final number of batches = " + bmp.dim)
        println("final batch means = " + bmp)
	println("final batch means variance = " + bmp.variance)
        println("mean of batch means = " + bmp.mean)
        
        println("***************************************")
        println("***************************************")
        println("***************************************")

        bmp.mean.asInstanceOf[Output]
    } // evaluate
*/

    /** Applies the function. If caching is enabled and the the given input
     *  has already been applied then the cached value is returned.
     */
    override def apply (x: Input) = 
    {
        println("Calling objective function... iters = " + iters + ", vMax = " + vMax)
        iters += 1
        if (enableCache) cache get (x) match {
	    case Some (value) => value
            case None         => {
                val value = f (x)
                cache put (x, value)
                value
	    }
        } else f (x)
    } // apply

    /** Readys the objective function for optimization by setting
     *  some maxima optimizer.
     */
    def using (o: OptType with MaximaOptimization) = 
    {
        new ObjFunc [Input, Output, OptType, MaximaOptimization] (f, Some(o))
    } // using

//
//    /** Readys the objective function for optimization by setting
//     *  some maxima optimizer.
//     */
//    def using (o: OptType with MaximaOptimization) = 
//    {
//        new ObjFunc [Input, Output, OptType, MaximaOptimization] (f, Some(o))
//    } // using

    /** Optimizes the objective function using the set optimizer.
     */
    def optimize (x0: Input) (vMax: Double) : (Input, Output) = 
    {
        this.vMax = vMax
        optimizer.get.setObjective[OptType, MinMax](this).solve(x0)
    } // optimize
  
} // ObjFunc

trait SimOptDSL 
{
    /** Implicitly enable the caching rule
     */
    implicit val enableCache = true

    /** Implicitly makes all VectorI => Double functions an ObjFunc
     */
    implicit def mkObjFuncI (f: VectorI => Double) 
        = new ObjFunc [VectorI, Double, IntegerOptimizer, UnreadyOptimization] (f)

    /** Implicitly makes all VectorD => Double functions an ObjFunc
     */
    implicit def mkObjFuncR (f: VectorD => Double) 
        = new ObjFunc [VectorD, Double, RealOptimizer, UnreadyOptimization] (f)

    /** Minimizes an objective function with some initial input.
     */
    def min [Input, Output, OptType <: Optimizer[Input, Output]] 
        (f: ObjFunc [Input, Output, OptType, MinimaOptimization]) 
        (x0: Input, tMax: Double = .2) =
    {
        f.optimize (x0) (tMax)
    } // minimize

    /** Maximizes an objective function with some initial input.
     */
    def max [Input, Output, OptType <: Optimizer[Input, Output]] 
        (f: ObjFunc [Input, Output, OptType, MaximaOptimization]) 
        (x0: Input, tMax: Double = .2) =
    {
        f.optimize (x0) (tMax)
    } // maximize
    
} // SimOptDSL

object SimOptDSLTest extends App with SimOptDSL
{
    def f (x: VectorI) = -(x(0)-5.)*(x(0)-5.) - x(1)*x(1)
    val x0 = new VectorI (1, 1)
    
    val results = max (f _ using new IntegerLocalOptimizer) (x0)
    println(results)

} // SimOptDSLTest
