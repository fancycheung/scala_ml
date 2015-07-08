/**
 * Created by fxzhang on 2015/7/6.
 */


class LogisticRegression(var learning:String="Gradient Decent",var iterate:Int=100,var learningRate:Double=0.001){

  def fit(X: Array[Array[Double]], y: Array[Int]) = {
    val x_row = X.length
    val x_column = X(0).length
    val theta =new Array[Double](x_column)
    println(this.iterate)
    println(this.learningRate)
    println(this.learning)

    for (i <- 0 until x_column){
      theta(i)=0.0
    }

    for (i_iterdate <- 1 to this.iterate) {
      for (j_index<-0 until x_column) {
        var sigma = 0.0
        for (i_index <- 0 until x_row) {
          sigma += (sigmod(X(i_index),theta) - y(i_index)) * X(i_index)(j_index)
        }
        theta(j_index)=theta(j_index)-this.learningRate*sigma/x_row
      }
      println {
        "for " + i_iterdate + " theta:"
      }
      for (i <- 0 until theta.length){
        print(theta(i))
        print("  ")
      }
      println("")
    }
    }

  def sigmod(x:Array[Double],theta:Array[Double])={
    var result=0.0
    for (i <- 0 until x.length){
      result+=x(i)*theta(i)
    }
    result
  }
}

