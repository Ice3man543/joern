package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

/** Verifies that calls whose receiver resolves (possibly transitively, via the internal
  * `<member>(P):M` form that [[io.joern.x2cpg.frontendspecific.jssrc2cpg.JavaScriptTypeRecovery]]
  * produces) to an external npm package `P` carry a canonical dotted form `"P.M"` in their
  * `dynamicTypeHintFullName`, in addition to the pre-existing internal `methodFullName`.
  */
class CanonicalExternalImportFullNameTests extends DataFlowCodeToCpgSuite {

  private val sequelizeCanonical = "sequelize.query"

  "canonical external package fullName hints" should {

    "emit canonical dotted form for ES module default import" in {
      val cpg = code("""
        |import Sequelize from 'sequelize';
        |const db = new Sequelize();
        |db.query('SELECT 1');
        |""".stripMargin)

      val List(queryCall) = cpg.call.nameExact("query").l
      queryCall.dynamicTypeHintFullName should contain(sequelizeCanonical)
    }

    "emit canonical dotted form for ES module named import" in {
      val cpg = code("""
        |import { Sequelize } from 'sequelize';
        |const db = new Sequelize();
        |db.query('SELECT 1');
        |""".stripMargin)

      val List(queryCall) = cpg.call.nameExact("query").l
      queryCall.dynamicTypeHintFullName should contain(sequelizeCanonical)
    }

    "emit canonical dotted form for ES module namespace import" in {
      val cpg = code("""
        |import * as S from 'sequelize';
        |const db = new S.Sequelize();
        |db.query('SELECT 1');
        |""".stripMargin)

      val List(queryCall) = cpg.call.nameExact("query").l
      queryCall.dynamicTypeHintFullName should contain(sequelizeCanonical)
    }

    "emit canonical dotted form for CommonJS require" in {
      val cpg = code("""
        |const { Sequelize } = require('sequelize');
        |const db = new Sequelize();
        |db.query('SELECT 1');
        |""".stripMargin)

      val List(queryCall) = cpg.call.nameExact("query").l
      queryCall.dynamicTypeHintFullName should contain(sequelizeCanonical)
    }

    "leave methodFullName unchanged" in {
      val cpg = code("""
        |import { Sequelize } from 'sequelize';
        |const db = new Sequelize();
        |db.query('SELECT 1');
        |""".stripMargin)

      val List(queryCall) = cpg.call.nameExact("query").l
      queryCall.methodFullName should not be sequelizeCanonical
    }

    "not emit canonical hints for local relative imports" in {
      val cpg = code(
        """
          |const local = require('./local');
          |local.frobnicate();
          |""".stripMargin,
        "index.js"
      ).moreCode(
        """
          |module.exports = {
          |  frobnicate: function () { return 1; }
          |};
          |""".stripMargin,
        "local.js"
      )

      val List(frobnicate) = cpg.call.nameExact("frobnicate").l
      frobnicate.dynamicTypeHintFullName.exists(_.matches("^[a-zA-Z_][\\w-]*\\.frobnicate$")) shouldBe false
    }
  }

}
