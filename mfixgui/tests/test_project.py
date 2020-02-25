
import sys
import os
import unittest
import math

import pytest

from mfixgui.project import Keyword, Equation, Project, FloatExp, main

myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(myPath, os.pardir, os.pardir))


class ProjectTest(unittest.TestCase):
    main()


class TestEquation(unittest.TestCase):
    def test_add(self):
        eq = Equation('2+5')
        self.assertEqual(float(eq), 7.0)

    def test_sub(self):
        eq = Equation('2-5')
        self.assertEqual(float(eq), -3.0)

    def test_mul(self):
        eq = Equation('2*5')
        self.assertEqual(float(eq), 10.0)

    def test_div(self):
        eq = Equation('10/2')
        self.assertEqual(float(eq), 5.0)

    def test_pow(self):
        eq = Equation('10**2')
        self.assertEqual(float(eq), 100)

    def test_pi(self):
        eq = Equation('10*pi / 15')
        self.assertAlmostEqual(float(eq), 10 * math.pi / 15)

    def test_e(self):
        eq = Equation("5*e + pi")
        self.assertAlmostEqual(float(eq), 5 * math.e + math.pi)

    def test_parens(self):
        eq = Equation("(1) + (2+3) * (5 - (6+7)) / 137.0")
        self.assertAlmostEqual(float(eq), 1 + (-40 / 137.0))

    def test_sin(self):
        eq = Equation("sin(pi)")
        self.assertAlmostEqual(float(eq), math.sin(math.pi))


class TestKeyword(unittest.TestCase):
    def test_attr(self):
        kw = Keyword('key', 'val', comment='comment', dtype=float, args=[1])
        self.assertEqual(kw.key, 'key')

    def test_int(self):
        kw = Keyword('key', 10)
        self.assertEqual(int(kw), 10)

    def test_float(self):
        kw = Keyword('key', 1.123)
        self.assertEqual(float(kw), 1.123)

    def test_cmp_eq(self):
        kw = Keyword('key', -100.1)
        self.assertEqual(kw, -100.1)

    def test_cmp_lt(self):
        kw = Keyword('key', 100.2)
        self.assertLess(kw, 1000)

    def test_cmp_gt(self):
        kw = Keyword('key', 100.3)
        self.assertGreater(kw, -100)

    def test_if(self):
        kw = Keyword('key', True)
        self.assertTrue(bool(kw))

    def test_eq(self):
        eq = Equation('2*5')
        kw = Keyword('key', eq)

        self.assertEqual(10.0, float(eq))
        self.assertEqual(10.0, float(kw))

    def test_update_value(self):
        kw = Keyword('key', True)

        for dtype, value in ((bool, True), (str, 'test'), (float, 10.0),
                             (int, 1), (float, None)):
            kw.dtype = dtype
            kw.updateValue(value)

            self.assertEqual(kw.value, value)

    def test_update_eq(self):
        kw = Keyword('key', Equation('2*4'))

        self.assertIsInstance(kw.value, Equation)

        kw.updateValue('10*2')
        self.assertIsInstance(kw.value, Equation)
        self.assertEqual(20.0, float(kw))

    def test_update_eq2(self):
        kw = Keyword('key', 2.4)

        self.assertEqual(float, kw.dtype)

        kw.updateValue('10*20')

        self.assertIsInstance(kw.value, Equation)
        self.assertEqual(10.0 * 20, float(kw))

    def test_lower(self):
        kw = Keyword('key', 'CamelCase')
        self.assertEqual('camelcase', kw.lower())


class TestParser(unittest.TestCase):
    def setUp(self):
        self.project = Project()

    def _test(self, line, expect=None):
        # Helper function, factoring out common stuff in parser test
        #  Tests for data type as well as value equality
        results = list(self.project.parseKeywordLine(line))
        self.assertEqual(len(results), len(expect))
        for (r, e) in zip(results, expect):
            self.assertEqual(len(r), len(e))
            for (r1, e1) in zip(r, e):
                self.assertEqual(type(r1), type(e1))
                if isinstance(e1, Equation):
                    self.assertEqual(float(e1), float(r1))
                else:
                    self.assertEqual(r1, e1)

    def test_parseKeywordLine_str(self):
        line = """key = 'value'"""
        expect = [('key', [], 'value')]
        self._test(line, expect)

    def test_parseKeywordLine_int(self):
        line = "key = 1"
        expect = [('key', [], 1)]
        self._test(line, expect)

    def test_parseKeywordLine_float(self):
        line = "key = 10.0"
        expect = [('key', [], 10.0)]
        self._test(line, expect)

    def test_parseKeywordLine_float_d(self):
        line = "key = 10.0 10d 10.d 10D 10.D 10.0D"
        expect = [('key', [i + 1], 10.0 if i == 0 else FloatExp(10))
                  for i in range(6)]
        self._test(line, expect)

    def test_parseKeywordLine_float_e(self):
        line = "key = 10.0 10e 10.e 10E 10.E 10.0E"
        expect = [('key', [i + 1], 10.0 if i == 0 else FloatExp(10.0))
                  for i in range(6)]
        self._test(line, expect)

    def test_parseKeywordLineShorthandEq(self):
        line = "key = @(2*5) 2*10 10 @(4+6) 10"
        expect = [('key', [i + 1], (Equation("2*5")
                                    if i == 0 else Equation("4+6")
                                    if i == 4 else 10)) for i in range(6)]
        self._test(line, expect)

    def test_parseKeywordLine_exp(self):
        line = "key = 1.0e10 1.d10 1.0e+10 1.d+10"
        expect = [('key', [i + 1], FloatExp(1e10)) for i in range(4)]
        self._test(line, expect)

    def test_parseKeywordLine_exp_neg(self):
        line = "key = 1.0e-10 1.d-10"
        expect = [('key', [i + 1], FloatExp(1e-10)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_bool(self):
        line = "key = .T."
        expect = [('key', [], True)]
        self._test(line, expect)

    def test_parseKeywordLine_arg(self):
        line = "key(3) = .T."
        expect = [('key', [3], True)]
        self._test(line, expect)

    def test_parseKeywordLine_comma_args(self):
        line = "key(3,2) = .T."
        expect = [('key', [3, 2], True)]
        self._test(line, expect)

    def test_parseKeywordLine_colon_args(self):
        line = "key(1:2) = .F. .T."
        expect = [('key', [i + 1], bool(i)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_complex_args(self):
        line = "key(1:2,55) = .F. .T."
        expect = [('key', [i + 1, 55], bool(i)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_complex_args_wspace(self):
        line = "key( 1 :2, 55) = .F. .T."
        expect = [('key', [i + 1, 55], bool(i)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_complex_args_wspace2(self):
        line = "key  ( 1 :2 , 55 ) = .F. .T."
        expect = [('key', [i + 1, 55], bool(i)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_complex_args2(self):
        line = "key(1,1:2,5) = .F. .T."
        expect = [('key', [1, i + 1, 5], bool(i)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_complex_args3(self):
        line = "key(1,5,1:2) = .F. .T."
        expect = [('key', [1, 5, i + 1], bool(i)) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_two_colons(self):
        line = "key(1,1:2,1:2) = .F. .T."
        with self.assertRaises(ValueError):
            self._test(line)

    def test_parseKeywordLine_colon_args_mismatch(self):
        line = "key(1:2) = .F. .T. .F."
        with self.assertRaises(ValueError):
            self._test(line, None)

    def test_parseKeywordLine_colon_args_mismatch2(self):
        line = "key(1:6) = .F. .T."
        with self.assertRaises(ValueError):
            self._test(line, None)

    def test_parseKeywordLine_eq_mul(self):
        line = "key = @(2*3)"
        expect = [('key', [], Equation('2*3'))]
        self._test(line, expect)

    def test_parseKeywordLine_eq_mul_wspace(self):
        line = "key = @( 2*3)"
        expect = [('key', [], Equation('2*3'))]
        self._test(line, expect)

    def test_parseKeywordLine_eq_divide(self):
        line = "key = @( 2/ 3)"
        expect = [('key', [], Equation('2/3'))]
        self._test(line, expect)

    def test_parseKeywordLine_indices_colon(self):
        # tests/dem/DEM01/mfix.dat
        line = "SPECIES_EQ(0:1) = .F.  .F."
        expect = [('species_eq', [i], False) for i in range(2)]
        self._test(line, expect)

    def test_parseKeywordLine_indices_compound1(self):
        # benchmarks/tfm/ParallelBenchmarkCases/C_COM_06/mfix.dat, simplfiied
        line = "BC_hw_X_s(10:15,1,2) = 6*0.0"
        expect = [('bc_hw_x_s', [i, 1, 2], 0.0) for i in range(10, 16)]
        self._test(line, expect)

    def test_parseKeywordLine_indices_compound2(self):
        # benchmarks/tfm/ParallelBenchmarkCases/C_COM_06/mfix.dat
        line = "BC_hw_X_s(10:15,1,2) = 6*0.0    BC_C_X_s(10:15,1,2) = 6*0.0"
        expect = [('bc_hw_x_s', [i, 1, 2], 0.0) for i in range(10, 16)] + \
                 [('bc_c_x_s', [i, 1, 2], 0.0) for i in range(10, 16)]
        self._test(line, expect)

    def test_parseKeywordLine_eq_pi(self):
        line = "key = @( 2*pi)"
        expect = [('key', [], Equation('2*pi'))]
        self._test(line, expect)

    def test_parseKeywordLine_shorthand_str(self):
        line = "key = 4*'ISIS'"
        expect = [('key', [i + 1], 'ISIS') for i in range(4)]
        self._test(line, expect)

    def test_parseKeywordLine_shorthand_float(self):
        line = "key = 4*6.7"
        expect = [('key', [i + 1], 6.7) for i in range(4)]
        self._test(line, expect)

    def test_parseKeywordLine_shorthand_float_other_val(self):
        line = "key = 4*6.7 6.7 6.7"
        expect = [('key', [i + 1], 6.7) for i in range(6)]
        self._test(line, expect)

    def test_parseKeywordLine_shorthand_eq(self):
        line = "key = 4*6.7 @(1*6.7)"
        expect = [('key', [i + 1], 6.7)
                  for i in range(4)] + [('key', [5], Equation('1*6.7'))]
        self._test(line, expect)

    def test_parseKeywordLine_shorthand_eq_2(self):
        line = "key =  @(2*5) 10 2*10 @(2*5) @(2*5)"
        expect = [('key', [i + 1], 10 if 0 < i < 4 else Equation("2*5"))
                  for i in range(6)]
        self._test(line, expect)

    def test_parseKeywordLine_shorthand_eq_3(self):
        line = "key =  @(5+ 5) 10 2*10 @(5+5) @ (5 + 5) "
        expect = [('key', [i + 1], 10 if 0 < i < 4 else Equation("5+5"))
                  for i in range(6)]
        self._test(line, expect)

    @pytest.mark.xfail(reason="should raise an error on bad input")
    def test_parseKeywordLine_shorthand_bad(self):
        line = "key =  10*10*10"
        self.assertRaises(ValueError)

    def test_parseKeywordLine_shorthand_eq_exp(self):
        line = "key = 4*6.7 @(1*6.7)"
        expect = [('key', [i + 1], 6.7)
                  for i in range(4)] + [('key', [5], Equation("1*6.7"))]
        self._test(line, expect)

    def test_parseKeywordLine_two_keys(self):
        line = "key0 = 10 key1 = 11"
        expect = [('key0', [], 10), ('key1', [], 11)]
        self._test(line, expect)

    def test_parsemfixdat_keyvalue(self):
        testString = """
                     key = 'value'
                     """

        project = Project(testString)

        self.assertEqual('value',
                         project.get_value('key'))


    def test_parsemfixdat_booleans(self):
        testString = """
                     truea = .True.
                     trueb = .T.
                     truec = .true.
                     trued = .t.
                     falsea = .False.
                     falseb = .F.
                     falsec = .false.
                     falsed = .f.
                     """

        project = Project(testString)

        expected = {
            'truea': True,
            'trueb': True,
            'truec': True,
            'trued': True,
            'falsea': False,
            'falseb': False,
            'falsec': False,
            'falsed': False,
        }

        for (key, expected_val) in expected.items():
            self.assertEqual(project.get_value(key), expected_val)

    def test_parsemfixdat_floats(self):
        testString = """
                     keya = 3.0
                     keyb = 3D0
                     keyc = 3.
                     keyd = 3.E5
                     """

        project = Project(testString)

        expected = {
            'keya': 3.0,
            'keyb': 3.0,
            'keyc': 3.0,
            'keyd': 3.0E5,
        }


        for key, expected_val in expected.items():
            self.assertEqual(float(project.get_value(key)), expected_val)


    def test_parsemfixdat_exponentialnotation(self):
        testString = """
                     keya = 3.0E-10
                     keyb = 1.1d10
                     """

        project = Project(testString)

        expected = {
            'keya': 3.0E-10,
            'keyb': 1.1E10,
        }


        for key, expected_val in expected.items():
            self.assertEqual(float(project.get_value(key)), expected_val)

    def test_parsemfixdat_equations(self):
        testString = """
                     ncy = 5 2*10 @(2*3)
                     """

        project = Project(testString)

        self.assertEqual('@(2*3)', str(project.variablegrid[4]['ncy']))

    def test_parsemfixdat_comments(self):
        testString = """
                     # comment before
                     keya = 'value' ! in line comment
                     keyb = 'value'# in line comment
                     ! comment after
                     """

        project = Project(testString)

        expected = {
            'keya': 'value',
            'keyb': 'value',
        }

        for key, expected_val in expected.items():
            self.assertEqual(project.get_value(key), expected_val)

    def test_parsemfixdat_commentedKeywords(self):
        testString = """
                     ! key = 3.0
                     # key = 'test'
                     """

        project = Project(testString)

        self.assertEqual(list(project.keywordItems()), [])

    def test_parsemfixdat_expandshorthand(self):
        testString = """
                     ic_ep_g = .4      1.0
                     leq_sweep = 3*'ISIS' 'JSJS'
                     ncx = 4*10
                     ncy = 5 2*10 @(2*3)
                     """

        newProject = Project(testString)

        # Check IC
        self.assertEqual(0.4, newProject.ics[1]['ic_ep_g'])
        self.assertEqual(1.0, newProject.ics[2]['ic_ep_g'])

        # Check Grid
        for i in range(1, 5):
            self.assertEqual(10, newProject.variablegrid[i]['ncx'])

        self.assertEqual(5, newProject.variablegrid[1]['ncy'])
        self.assertEqual(10, newProject.variablegrid[2]['ncy'])
        self.assertEqual(10, newProject.variablegrid[3]['ncy'])
        self.assertEqual('@(2*3)', str(newProject.variablegrid[4]['ncy']))

    def test_parsemfixdat_argswithmultiplevalues(self):
        testString = """
                     ic_ep_g(2) = .4      1.0
                     ic_rop_s(2,1) = 10 30
                     """

        newProject = Project(testString)

        # Check IC
        self.assertEqual(0.4, newProject.ics[2]['ic_ep_g'])
        self.assertEqual(1.0, newProject.ics[3]['ic_ep_g'])
        self.assertEqual(10, newProject.ics[2].solids[1]['ic_rop_s'])
        self.assertEqual(30, newProject.ics[3].solids[1]['ic_rop_s'])

    def test_parsemfixdat_ic(self):
        testString = """
                     ic_x_w(1) = 2.0
                     ic_x_g(1,2) = 0.3
                     ic_t_s(1,1) = 300.0
                     """

        newProject = Project(testString)

        self.assertEqual(0.3, newProject.ics[1].gasSpecies[2]['ic_x_g'])
        self.assertEqual(2.0, newProject.ics[1]['ic_x_w'])
        self.assertEqual(300.0, newProject.ics[1].solids[1]['ic_t_s'])

    def test_parsemfixdat_bc(self):
        testString = """
                     bc_x_w(1) = 2.0
                     bc_x_g(1,2) = 0.3
                     bc_t_s(1,1) = 300.0
                     """

        newProject = Project(testString)

        self.assertEqual(0.3, newProject.bcs[1].gasSpecies[2]['bc_x_g'])
        self.assertEqual(2.0, newProject.bcs[1]['bc_x_w'])
        self.assertEqual(300.0, newProject.bcs[1].solids[1]['bc_t_s'])

    def test_parsemfixdat_ps(self):
        testString = """
                     ps_x_w(1) = 2.0
                     ps_x_g(1,2) = 0.3
                     ps_t_s(1,1) = 300.0
                     """

        newProject = Project(testString)

        self.assertEqual(0.3, newProject.pss[1].gasSpecies[2]['ps_x_g'])
        self.assertEqual(2.0, newProject.pss[1]['ps_x_w'])
        self.assertEqual(300.0, newProject.pss[1].solids[1]['ps_t_s'])
