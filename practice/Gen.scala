
// Version for 8.3
trait Prop { 
  def check: Boolean 

  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}
