package org.kiji.modeling

import org.kiji.avro.dsl.AvroSchemaParser


object AvroConf {


  val parser = new AvroSchemaParser()

  val schema = parser.parseSequence("""
    |record ns.A {int integer}
    |record ns.B {
    |  union { ns.A, null } field = ns.A { integer = 0 }
    |}
  """.stripMargin)


  def main(args: Array[String]) = {
    println(schema)
  }

}
