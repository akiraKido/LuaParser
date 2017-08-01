package AST

abstract class LuaObject{
  def value : Any
  def generateCSharpScript() : String

  override def toString: String = generateCSharpScript()
}

class LuaIdentifier(_value:String) extends LuaObject{
  override def value: String = _value
  override def generateCSharpScript(): String = _value
}

abstract case class LuaExpression() extends LuaObject {
  def _type : String
}

class LuaNumber(_value:Float) extends LuaExpression{
  override def value: Float = _value

  override def generateCSharpScript(): String = value.toString

  override def _type: String = "float"
}

class LuaString(_value:String) extends LuaExpression {
  override def value: String = _value

  override def generateCSharpScript(): String = s""""$value""""

  override def _type: String = "string"
}


class LuaAssignmentExpression(_name:String, _expression: LuaExpression) extends LuaObject {
  override def value: String = s"""${_name} = ${_expression}"""

  def _type:String = _expression._type

  override def generateCSharpScript(): String = s"""${_type} ${_name} = ${_expression.generateCSharpScript()}"""
}

class LuaBinopExpression(_LHS:LuaObject, _binop:String, _RHS:LuaObject) extends LuaExpression {
  (_LHS, _RHS) match {
    case item: (LuaExpression, LuaExpression) => {
      if (item._1._type != item._2._type) throw new IllegalArgumentException(s"expected type:${item._1._type} but found type:${item._2._type}")
    }
  }

  override def value: Any = generateCSharpScript()

  override def generateCSharpScript(): String = s"""${_LHS.generateCSharpScript()} ${_binop} ${_RHS.generateCSharpScript()}"""

  override def _type: String = _LHS.asInstanceOf[LuaExpression]._type
}
