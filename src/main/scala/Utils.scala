
/**
 * @author rohitsinha
 */
package uclid {
  
  object Utils {
      def assert(b: Boolean, err: String) : Unit = {
        if (!b) { println("ERROR: " + err); System.exit(0); } //throw new Exception(err); }
      }
      
      class UnimplementedException (msg:String=null, cause:Throwable=null) extends java.lang.Exception (msg, cause) {}
      
      def existsOnce(a: List[lang.UclIdentifier], b: lang.UclIdentifier) : Boolean = existsNTimes(a,b,1)
      def existsNone(a: List[lang.UclIdentifier], b: lang.UclIdentifier) : Boolean = existsNTimes(a,b,0)
      def existsNTimes(a: List[lang.UclIdentifier], b: lang.UclIdentifier, n: Int) : Boolean = 
        a.count { x => x.value == b.value } == n
      
      def allUnique(a: List[lang.UclIdentifier]) : Boolean = a.distinct.size == a.size
  }
}