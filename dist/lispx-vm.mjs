/******/ var __webpack_modules__ = ({

/***/ "./node_modules/big.js/big.js":
/*!************************************!*\
  !*** ./node_modules/big.js/big.js ***!
  \************************************/
/***/ (function(module, exports, __webpack_require__) {

var __WEBPACK_AMD_DEFINE_RESULT__;/*
 *  big.js v6.1.1
 *  A small, fast, easy-to-use library for arbitrary-precision decimal arithmetic.
 *  Copyright (c) 2021 Michael Mclaughlin
 *  https://github.com/MikeMcl/big.js/LICENCE.md
 */
;(function (GLOBAL) {
  'use strict';
  var Big,


/************************************** EDITABLE DEFAULTS *****************************************/


    // The default values below must be integers within the stated ranges.

    /*
     * The maximum number of decimal places (DP) of the results of operations involving division:
     * div and sqrt, and pow with negative exponents.
     */
    DP = 20,            // 0 to MAX_DP

    /*
     * The rounding mode (RM) used when rounding to the above decimal places.
     *
     *  0  Towards zero (i.e. truncate, no rounding).       (ROUND_DOWN)
     *  1  To nearest neighbour. If equidistant, round up.  (ROUND_HALF_UP)
     *  2  To nearest neighbour. If equidistant, to even.   (ROUND_HALF_EVEN)
     *  3  Away from zero.                                  (ROUND_UP)
     */
    RM = 1,             // 0, 1, 2 or 3

    // The maximum value of DP and Big.DP.
    MAX_DP = 1E6,       // 0 to 1000000

    // The maximum magnitude of the exponent argument to the pow method.
    MAX_POWER = 1E6,    // 1 to 1000000

    /*
     * The negative exponent (NE) at and beneath which toString returns exponential notation.
     * (JavaScript numbers: -7)
     * -1000000 is the minimum recommended exponent value of a Big.
     */
    NE = -7,            // 0 to -1000000

    /*
     * The positive exponent (PE) at and above which toString returns exponential notation.
     * (JavaScript numbers: 21)
     * 1000000 is the maximum recommended exponent value of a Big, but this limit is not enforced.
     */
    PE = 21,            // 0 to 1000000

    /*
     * When true, an error will be thrown if a primitive number is passed to the Big constructor,
     * or if valueOf is called, or if toNumber is called on a Big which cannot be converted to a
     * primitive number without a loss of precision.
     */
    STRICT = false,     // true or false


/**************************************************************************************************/


    // Error messages.
    NAME = '[big.js] ',
    INVALID = NAME + 'Invalid ',
    INVALID_DP = INVALID + 'decimal places',
    INVALID_RM = INVALID + 'rounding mode',
    DIV_BY_ZERO = NAME + 'Division by zero',

    // The shared prototype object.
    P = {},
    UNDEFINED = void 0,
    NUMERIC = /^-?(\d+(\.\d*)?|\.\d+)(e[+-]?\d+)?$/i;


  /*
   * Create and return a Big constructor.
   */
  function _Big_() {

    /*
     * The Big constructor and exported function.
     * Create and return a new instance of a Big number object.
     *
     * n {number|string|Big} A numeric value.
     */
    function Big(n) {
      var x = this;

      // Enable constructor usage without new.
      if (!(x instanceof Big)) return n === UNDEFINED ? _Big_() : new Big(n);

      // Duplicate.
      if (n instanceof Big) {
        x.s = n.s;
        x.e = n.e;
        x.c = n.c.slice();
      } else {
        if (typeof n !== 'string') {
          if (Big.strict === true) {
            throw TypeError(INVALID + 'number');
          }

          // Minus zero?
          n = n === 0 && 1 / n < 0 ? '-0' : String(n);
        }

        parse(x, n);
      }

      // Retain a reference to this Big constructor.
      // Shadow Big.prototype.constructor which points to Object.
      x.constructor = Big;
    }

    Big.prototype = P;
    Big.DP = DP;
    Big.RM = RM;
    Big.NE = NE;
    Big.PE = PE;
    Big.strict = STRICT;
    Big.roundDown = 0;
    Big.roundHalfUp = 1;
    Big.roundHalfEven = 2;
    Big.roundUp = 3;

    return Big;
  }


  /*
   * Parse the number or string value passed to a Big constructor.
   *
   * x {Big} A Big number instance.
   * n {number|string} A numeric value.
   */
  function parse(x, n) {
    var e, i, nl;

    if (!NUMERIC.test(n)) {
      throw Error(INVALID + 'number');
    }

    // Determine sign.
    x.s = n.charAt(0) == '-' ? (n = n.slice(1), -1) : 1;

    // Decimal point?
    if ((e = n.indexOf('.')) > -1) n = n.replace('.', '');

    // Exponential form?
    if ((i = n.search(/e/i)) > 0) {

      // Determine exponent.
      if (e < 0) e = i;
      e += +n.slice(i + 1);
      n = n.substring(0, i);
    } else if (e < 0) {

      // Integer.
      e = n.length;
    }

    nl = n.length;

    // Determine leading zeros.
    for (i = 0; i < nl && n.charAt(i) == '0';) ++i;

    if (i == nl) {

      // Zero.
      x.c = [x.e = 0];
    } else {

      // Determine trailing zeros.
      for (; nl > 0 && n.charAt(--nl) == '0';);
      x.e = e - i - 1;
      x.c = [];

      // Convert string to array of digits without leading/trailing zeros.
      for (e = 0; i <= nl;) x.c[e++] = +n.charAt(i++);
    }

    return x;
  }


  /*
   * Round Big x to a maximum of sd significant digits using rounding mode rm.
   *
   * x {Big} The Big to round.
   * sd {number} Significant digits: integer, 0 to MAX_DP inclusive.
   * rm {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   * [more] {boolean} Whether the result of division was truncated.
   */
  function round(x, sd, rm, more) {
    var xc = x.c;

    if (rm === UNDEFINED) rm = x.constructor.RM;
    if (rm !== 0 && rm !== 1 && rm !== 2 && rm !== 3) {
      throw Error(INVALID_RM);
    }

    if (sd < 1) {
      more =
        rm === 3 && (more || !!xc[0]) || sd === 0 && (
        rm === 1 && xc[0] >= 5 ||
        rm === 2 && (xc[0] > 5 || xc[0] === 5 && (more || xc[1] !== UNDEFINED))
      );

      xc.length = 1;

      if (more) {

        // 1, 0.1, 0.01, 0.001, 0.0001 etc.
        x.e = x.e - sd + 1;
        xc[0] = 1;
      } else {

        // Zero.
        xc[0] = x.e = 0;
      }
    } else if (sd < xc.length) {

      // xc[sd] is the digit after the digit that may be rounded up.
      more =
        rm === 1 && xc[sd] >= 5 ||
        rm === 2 && (xc[sd] > 5 || xc[sd] === 5 &&
          (more || xc[sd + 1] !== UNDEFINED || xc[sd - 1] & 1)) ||
        rm === 3 && (more || !!xc[0]);

      // Remove any digits after the required precision.
      xc.length = sd--;

      // Round up?
      if (more) {

        // Rounding up may mean the previous digit has to be rounded up.
        for (; ++xc[sd] > 9;) {
          xc[sd] = 0;
          if (!sd--) {
            ++x.e;
            xc.unshift(1);
          }
        }
      }

      // Remove trailing zeros.
      for (sd = xc.length; !xc[--sd];) xc.pop();
    }

    return x;
  }


  /*
   * Return a string representing the value of Big x in normal or exponential notation.
   * Handles P.toExponential, P.toFixed, P.toJSON, P.toPrecision, P.toString and P.valueOf.
   */
  function stringify(x, doExponential, isNonzero) {
    var e = x.e,
      s = x.c.join(''),
      n = s.length;

    // Exponential notation?
    if (doExponential) {
      s = s.charAt(0) + (n > 1 ? '.' + s.slice(1) : '') + (e < 0 ? 'e' : 'e+') + e;

    // Normal notation.
    } else if (e < 0) {
      for (; ++e;) s = '0' + s;
      s = '0.' + s;
    } else if (e > 0) {
      if (++e > n) {
        for (e -= n; e--;) s += '0';
      } else if (e < n) {
        s = s.slice(0, e) + '.' + s.slice(e);
      }
    } else if (n > 1) {
      s = s.charAt(0) + '.' + s.slice(1);
    }

    return x.s < 0 && isNonzero ? '-' + s : s;
  }


  // Prototype/instance methods


  /*
   * Return a new Big whose value is the absolute value of this Big.
   */
  P.abs = function () {
    var x = new this.constructor(this);
    x.s = 1;
    return x;
  };


  /*
   * Return 1 if the value of this Big is greater than the value of Big y,
   *       -1 if the value of this Big is less than the value of Big y, or
   *        0 if they have the same value.
   */
  P.cmp = function (y) {
    var isneg,
      x = this,
      xc = x.c,
      yc = (y = new x.constructor(y)).c,
      i = x.s,
      j = y.s,
      k = x.e,
      l = y.e;

    // Either zero?
    if (!xc[0] || !yc[0]) return !xc[0] ? !yc[0] ? 0 : -j : i;

    // Signs differ?
    if (i != j) return i;

    isneg = i < 0;

    // Compare exponents.
    if (k != l) return k > l ^ isneg ? 1 : -1;

    j = (k = xc.length) < (l = yc.length) ? k : l;

    // Compare digit by digit.
    for (i = -1; ++i < j;) {
      if (xc[i] != yc[i]) return xc[i] > yc[i] ^ isneg ? 1 : -1;
    }

    // Compare lengths.
    return k == l ? 0 : k > l ^ isneg ? 1 : -1;
  };


  /*
   * Return a new Big whose value is the value of this Big divided by the value of Big y, rounded,
   * if necessary, to a maximum of Big.DP decimal places using rounding mode Big.RM.
   */
  P.div = function (y) {
    var x = this,
      Big = x.constructor,
      a = x.c,                  // dividend
      b = (y = new Big(y)).c,   // divisor
      k = x.s == y.s ? 1 : -1,
      dp = Big.DP;

    if (dp !== ~~dp || dp < 0 || dp > MAX_DP) {
      throw Error(INVALID_DP);
    }

    // Divisor is zero?
    if (!b[0]) {
      throw Error(DIV_BY_ZERO);
    }

    // Dividend is 0? Return +-0.
    if (!a[0]) {
      y.s = k;
      y.c = [y.e = 0];
      return y;
    }

    var bl, bt, n, cmp, ri,
      bz = b.slice(),
      ai = bl = b.length,
      al = a.length,
      r = a.slice(0, bl),   // remainder
      rl = r.length,
      q = y,                // quotient
      qc = q.c = [],
      qi = 0,
      p = dp + (q.e = x.e - y.e) + 1;    // precision of the result

    q.s = k;
    k = p < 0 ? 0 : p;

    // Create version of divisor with leading zero.
    bz.unshift(0);

    // Add zeros to make remainder as long as divisor.
    for (; rl++ < bl;) r.push(0);

    do {

      // n is how many times the divisor goes into current remainder.
      for (n = 0; n < 10; n++) {

        // Compare divisor and remainder.
        if (bl != (rl = r.length)) {
          cmp = bl > rl ? 1 : -1;
        } else {
          for (ri = -1, cmp = 0; ++ri < bl;) {
            if (b[ri] != r[ri]) {
              cmp = b[ri] > r[ri] ? 1 : -1;
              break;
            }
          }
        }

        // If divisor < remainder, subtract divisor from remainder.
        if (cmp < 0) {

          // Remainder can't be more than 1 digit longer than divisor.
          // Equalise lengths using divisor with extra leading zero?
          for (bt = rl == bl ? b : bz; rl;) {
            if (r[--rl] < bt[rl]) {
              ri = rl;
              for (; ri && !r[--ri];) r[ri] = 9;
              --r[ri];
              r[rl] += 10;
            }
            r[rl] -= bt[rl];
          }

          for (; !r[0];) r.shift();
        } else {
          break;
        }
      }

      // Add the digit n to the result array.
      qc[qi++] = cmp ? n : ++n;

      // Update the remainder.
      if (r[0] && cmp) r[rl] = a[ai] || 0;
      else r = [a[ai]];

    } while ((ai++ < al || r[0] !== UNDEFINED) && k--);

    // Leading zero? Do not remove if result is simply zero (qi == 1).
    if (!qc[0] && qi != 1) {

      // There can't be more than one zero.
      qc.shift();
      q.e--;
      p--;
    }

    // Round?
    if (qi > p) round(q, p, Big.RM, r[0] !== UNDEFINED);

    return q;
  };


  /*
   * Return true if the value of this Big is equal to the value of Big y, otherwise return false.
   */
  P.eq = function (y) {
    return this.cmp(y) === 0;
  };


  /*
   * Return true if the value of this Big is greater than the value of Big y, otherwise return
   * false.
   */
  P.gt = function (y) {
    return this.cmp(y) > 0;
  };


  /*
   * Return true if the value of this Big is greater than or equal to the value of Big y, otherwise
   * return false.
   */
  P.gte = function (y) {
    return this.cmp(y) > -1;
  };


  /*
   * Return true if the value of this Big is less than the value of Big y, otherwise return false.
   */
  P.lt = function (y) {
    return this.cmp(y) < 0;
  };


  /*
   * Return true if the value of this Big is less than or equal to the value of Big y, otherwise
   * return false.
   */
  P.lte = function (y) {
    return this.cmp(y) < 1;
  };


  /*
   * Return a new Big whose value is the value of this Big minus the value of Big y.
   */
  P.minus = P.sub = function (y) {
    var i, j, t, xlty,
      x = this,
      Big = x.constructor,
      a = x.s,
      b = (y = new Big(y)).s;

    // Signs differ?
    if (a != b) {
      y.s = -b;
      return x.plus(y);
    }

    var xc = x.c.slice(),
      xe = x.e,
      yc = y.c,
      ye = y.e;

    // Either zero?
    if (!xc[0] || !yc[0]) {
      if (yc[0]) {
        y.s = -b;
      } else if (xc[0]) {
        y = new Big(x);
      } else {
        y.s = 1;
      }
      return y;
    }

    // Determine which is the bigger number. Prepend zeros to equalise exponents.
    if (a = xe - ye) {

      if (xlty = a < 0) {
        a = -a;
        t = xc;
      } else {
        ye = xe;
        t = yc;
      }

      t.reverse();
      for (b = a; b--;) t.push(0);
      t.reverse();
    } else {

      // Exponents equal. Check digit by digit.
      j = ((xlty = xc.length < yc.length) ? xc : yc).length;

      for (a = b = 0; b < j; b++) {
        if (xc[b] != yc[b]) {
          xlty = xc[b] < yc[b];
          break;
        }
      }
    }

    // x < y? Point xc to the array of the bigger number.
    if (xlty) {
      t = xc;
      xc = yc;
      yc = t;
      y.s = -y.s;
    }

    /*
     * Append zeros to xc if shorter. No need to add zeros to yc if shorter as subtraction only
     * needs to start at yc.length.
     */
    if ((b = (j = yc.length) - (i = xc.length)) > 0) for (; b--;) xc[i++] = 0;

    // Subtract yc from xc.
    for (b = i; j > a;) {
      if (xc[--j] < yc[j]) {
        for (i = j; i && !xc[--i];) xc[i] = 9;
        --xc[i];
        xc[j] += 10;
      }

      xc[j] -= yc[j];
    }

    // Remove trailing zeros.
    for (; xc[--b] === 0;) xc.pop();

    // Remove leading zeros and adjust exponent accordingly.
    for (; xc[0] === 0;) {
      xc.shift();
      --ye;
    }

    if (!xc[0]) {

      // n - n = +0
      y.s = 1;

      // Result must be zero.
      xc = [ye = 0];
    }

    y.c = xc;
    y.e = ye;

    return y;
  };


  /*
   * Return a new Big whose value is the value of this Big modulo the value of Big y.
   */
  P.mod = function (y) {
    var ygtx,
      x = this,
      Big = x.constructor,
      a = x.s,
      b = (y = new Big(y)).s;

    if (!y.c[0]) {
      throw Error(DIV_BY_ZERO);
    }

    x.s = y.s = 1;
    ygtx = y.cmp(x) == 1;
    x.s = a;
    y.s = b;

    if (ygtx) return new Big(x);

    a = Big.DP;
    b = Big.RM;
    Big.DP = Big.RM = 0;
    x = x.div(y);
    Big.DP = a;
    Big.RM = b;

    return this.minus(x.times(y));
  };


  /*
   * Return a new Big whose value is the value of this Big plus the value of Big y.
   */
  P.plus = P.add = function (y) {
    var e, k, t,
      x = this,
      Big = x.constructor;

    y = new Big(y);

    // Signs differ?
    if (x.s != y.s) {
      y.s = -y.s;
      return x.minus(y);
    }

    var xe = x.e,
      xc = x.c,
      ye = y.e,
      yc = y.c;

    // Either zero?
    if (!xc[0] || !yc[0]) {
      if (!yc[0]) {
        if (xc[0]) {
          y = new Big(x);
        } else {
          y.s = x.s;
        }
      }
      return y;
    }

    xc = xc.slice();

    // Prepend zeros to equalise exponents.
    // Note: reverse faster than unshifts.
    if (e = xe - ye) {
      if (e > 0) {
        ye = xe;
        t = yc;
      } else {
        e = -e;
        t = xc;
      }

      t.reverse();
      for (; e--;) t.push(0);
      t.reverse();
    }

    // Point xc to the longer array.
    if (xc.length - yc.length < 0) {
      t = yc;
      yc = xc;
      xc = t;
    }

    e = yc.length;

    // Only start adding at yc.length - 1 as the further digits of xc can be left as they are.
    for (k = 0; e; xc[e] %= 10) k = (xc[--e] = xc[e] + yc[e] + k) / 10 | 0;

    // No need to check for zero, as +x + +y != 0 && -x + -y != 0

    if (k) {
      xc.unshift(k);
      ++ye;
    }

    // Remove trailing zeros.
    for (e = xc.length; xc[--e] === 0;) xc.pop();

    y.c = xc;
    y.e = ye;

    return y;
  };


  /*
   * Return a Big whose value is the value of this Big raised to the power n.
   * If n is negative, round to a maximum of Big.DP decimal places using rounding
   * mode Big.RM.
   *
   * n {number} Integer, -MAX_POWER to MAX_POWER inclusive.
   */
  P.pow = function (n) {
    var x = this,
      one = new x.constructor('1'),
      y = one,
      isneg = n < 0;

    if (n !== ~~n || n < -MAX_POWER || n > MAX_POWER) {
      throw Error(INVALID + 'exponent');
    }

    if (isneg) n = -n;

    for (;;) {
      if (n & 1) y = y.times(x);
      n >>= 1;
      if (!n) break;
      x = x.times(x);
    }

    return isneg ? one.div(y) : y;
  };


  /*
   * Return a new Big whose value is the value of this Big rounded to a maximum precision of sd
   * significant digits using rounding mode rm, or Big.RM if rm is not specified.
   *
   * sd {number} Significant digits: integer, 1 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.prec = function (sd, rm) {
    if (sd !== ~~sd || sd < 1 || sd > MAX_DP) {
      throw Error(INVALID + 'precision');
    }
    return round(new this.constructor(this), sd, rm);
  };


  /*
   * Return a new Big whose value is the value of this Big rounded to a maximum of dp decimal places
   * using rounding mode rm, or Big.RM if rm is not specified.
   * If dp is negative, round to an integer which is a multiple of 10**-dp.
   * If dp is not specified, round to 0 decimal places.
   *
   * dp? {number} Integer, -MAX_DP to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.round = function (dp, rm) {
    if (dp === UNDEFINED) dp = 0;
    else if (dp !== ~~dp || dp < -MAX_DP || dp > MAX_DP) {
      throw Error(INVALID_DP);
    }
    return round(new this.constructor(this), dp + this.e + 1, rm);
  };


  /*
   * Return a new Big whose value is the square root of the value of this Big, rounded, if
   * necessary, to a maximum of Big.DP decimal places using rounding mode Big.RM.
   */
  P.sqrt = function () {
    var r, c, t,
      x = this,
      Big = x.constructor,
      s = x.s,
      e = x.e,
      half = new Big('0.5');

    // Zero?
    if (!x.c[0]) return new Big(x);

    // Negative?
    if (s < 0) {
      throw Error(NAME + 'No square root');
    }

    // Estimate.
    s = Math.sqrt(x + '');

    // Math.sqrt underflow/overflow?
    // Re-estimate: pass x coefficient to Math.sqrt as integer, then adjust the result exponent.
    if (s === 0 || s === 1 / 0) {
      c = x.c.join('');
      if (!(c.length + e & 1)) c += '0';
      s = Math.sqrt(c);
      e = ((e + 1) / 2 | 0) - (e < 0 || e & 1);
      r = new Big((s == 1 / 0 ? '5e' : (s = s.toExponential()).slice(0, s.indexOf('e') + 1)) + e);
    } else {
      r = new Big(s + '');
    }

    e = r.e + (Big.DP += 4);

    // Newton-Raphson iteration.
    do {
      t = r;
      r = half.times(t.plus(x.div(t)));
    } while (t.c.slice(0, e).join('') !== r.c.slice(0, e).join(''));

    return round(r, (Big.DP -= 4) + r.e + 1, Big.RM);
  };


  /*
   * Return a new Big whose value is the value of this Big times the value of Big y.
   */
  P.times = P.mul = function (y) {
    var c,
      x = this,
      Big = x.constructor,
      xc = x.c,
      yc = (y = new Big(y)).c,
      a = xc.length,
      b = yc.length,
      i = x.e,
      j = y.e;

    // Determine sign of result.
    y.s = x.s == y.s ? 1 : -1;

    // Return signed 0 if either 0.
    if (!xc[0] || !yc[0]) {
      y.c = [y.e = 0];
      return y;
    }

    // Initialise exponent of result as x.e + y.e.
    y.e = i + j;

    // If array xc has fewer digits than yc, swap xc and yc, and lengths.
    if (a < b) {
      c = xc;
      xc = yc;
      yc = c;
      j = a;
      a = b;
      b = j;
    }

    // Initialise coefficient array of result with zeros.
    for (c = new Array(j = a + b); j--;) c[j] = 0;

    // Multiply.

    // i is initially xc.length.
    for (i = b; i--;) {
      b = 0;

      // a is yc.length.
      for (j = a + i; j > i;) {

        // Current sum of products at this digit position, plus carry.
        b = c[j] + yc[i] * xc[j - i - 1] + b;
        c[j--] = b % 10;

        // carry
        b = b / 10 | 0;
      }

      c[j] = b;
    }

    // Increment result exponent if there is a final carry, otherwise remove leading zero.
    if (b) ++y.e;
    else c.shift();

    // Remove trailing zeros.
    for (i = c.length; !c[--i];) c.pop();
    y.c = c;

    return y;
  };


  /*
   * Return a string representing the value of this Big in exponential notation rounded to dp fixed
   * decimal places using rounding mode rm, or Big.RM if rm is not specified.
   *
   * dp? {number} Decimal places: integer, 0 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.toExponential = function (dp, rm) {
    var x = this,
      n = x.c[0];

    if (dp !== UNDEFINED) {
      if (dp !== ~~dp || dp < 0 || dp > MAX_DP) {
        throw Error(INVALID_DP);
      }
      x = round(new x.constructor(x), ++dp, rm);
      for (; x.c.length < dp;) x.c.push(0);
    }

    return stringify(x, true, !!n);
  };


  /*
   * Return a string representing the value of this Big in normal notation rounded to dp fixed
   * decimal places using rounding mode rm, or Big.RM if rm is not specified.
   *
   * dp? {number} Decimal places: integer, 0 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   *
   * (-0).toFixed(0) is '0', but (-0.1).toFixed(0) is '-0'.
   * (-0).toFixed(1) is '0.0', but (-0.01).toFixed(1) is '-0.0'.
   */
  P.toFixed = function (dp, rm) {
    var x = this,
      n = x.c[0];

    if (dp !== UNDEFINED) {
      if (dp !== ~~dp || dp < 0 || dp > MAX_DP) {
        throw Error(INVALID_DP);
      }
      x = round(new x.constructor(x), dp + x.e + 1, rm);

      // x.e may have changed if the value is rounded up.
      for (dp = dp + x.e + 1; x.c.length < dp;) x.c.push(0);
    }

    return stringify(x, false, !!n);
  };


  /*
   * Return a string representing the value of this Big.
   * Return exponential notation if this Big has a positive exponent equal to or greater than
   * Big.PE, or a negative exponent equal to or less than Big.NE.
   * Omit the sign for negative zero.
   */
  P.toJSON = P.toString = function () {
    var x = this,
      Big = x.constructor;
    return stringify(x, x.e <= Big.NE || x.e >= Big.PE, !!x.c[0]);
  };


  /*
   * Return the value of this Big as a primitve number.
   */
  P.toNumber = function () {
    var n = Number(stringify(this, true, true));
    if (this.constructor.strict === true && !this.eq(n.toString())) {
      throw Error(NAME + 'Imprecise conversion');
    }
    return n;
  };


  /*
   * Return a string representing the value of this Big rounded to sd significant digits using
   * rounding mode rm, or Big.RM if rm is not specified.
   * Use exponential notation if sd is less than the number of digits necessary to represent
   * the integer part of the value in normal notation.
   *
   * sd {number} Significant digits: integer, 1 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.toPrecision = function (sd, rm) {
    var x = this,
      Big = x.constructor,
      n = x.c[0];

    if (sd !== UNDEFINED) {
      if (sd !== ~~sd || sd < 1 || sd > MAX_DP) {
        throw Error(INVALID + 'precision');
      }
      x = round(new Big(x), sd, rm);
      for (; x.c.length < sd;) x.c.push(0);
    }

    return stringify(x, sd <= x.e || x.e <= Big.NE || x.e >= Big.PE, !!n);
  };


  /*
   * Return a string representing the value of this Big.
   * Return exponential notation if this Big has a positive exponent equal to or greater than
   * Big.PE, or a negative exponent equal to or less than Big.NE.
   * Include the sign for negative zero.
   */
  P.valueOf = function () {
    var x = this,
      Big = x.constructor;
    if (Big.strict === true) {
      throw Error(NAME + 'valueOf disallowed');
    }
    return stringify(x, x.e <= Big.NE || x.e >= Big.PE, true);
  };


  // Export


  Big = _Big_();

  Big['default'] = Big.Big = Big;

  //AMD.
  if (true) {
    !(__WEBPACK_AMD_DEFINE_RESULT__ = (function () { return Big; }).call(exports, __webpack_require__, exports, module),
		__WEBPACK_AMD_DEFINE_RESULT__ !== undefined && (module.exports = __WEBPACK_AMD_DEFINE_RESULT__));

  // Node and other CommonJS-like environments that support module.exports.
  } else {}
})(this);


/***/ }),

/***/ "./src/boot.lispx":
/*!************************!*\
  !*** ./src/boot.lispx ***!
  \************************/
/***/ ((module) => {

module.exports = ";;; LispX Bootstrap\n\n;; Copyright (c) 2021, 2022 Manuel J. Simoni\n\n;;; Core Forms\n\n(%%def #'list\n  (%%wrap (%%vau arguments #ignore arguments))\n\n  \"Return the list of evaluated ARGUMENTS.\n\n$(fn (arguments))\n$(type function)\")\n\n(%%def #'vau\n  (%%vau (parameter-tree environment-parameter . forms) env\n    (%%eval (list #'%%vau parameter-tree environment-parameter\n                  (%%list* #'%%progn forms))\n            env))\n\n  \"Construct a fexpr with the given PARAMETER-TREE,\nENVIRONMENT-PARAMETER, and FORMS.\n\n$(fn (parameter-tree environment-parameter . forms))\n$(type fexpr)\")\n\n(%%def #'%make-macro\n  (%%wrap\n   (%%vau (expander) #ignore\n     (%%vau form env\n       (%%eval\n        (%%eval (%%cons expander form) (%%make-environment))\n        env))))\n\n  \"Create a macro from an EXPANDER operator.  A macro is an operator\nthat receives a form and produces an expression (by calling the\nexpander with the form as argument) that is then evaluated in place of\nthe form.\n\n$(fn (expander))\n$(type function)\")\n\n(%%def #'macro\n  (%make-macro\n   (%%vau (parameter-tree . forms) #ignore\n     (list #'%make-macro\n           (%%list* #'vau parameter-tree #ignore forms))))\n\n  \"Create an anonymous macro with the given PARAMETER-TREE and FORMS.\n\n$(fn (parameter-tree . forms))\n$(type macro)\")\n\n(%%def #'defmacro\n  (macro (name parameter-tree . forms)\n    (list #'%%def (%%function-symbol name)\n          (%%list* #'macro parameter-tree forms)))\n\n  \"Define a macro with the given NAME, PARAMETER-TREE, and FORMS.\n\n$(fn (name parameter-tree . forms))\n$(type macro)\")\n\n(defmacro defexpr (name parameter-tree environment-parameter . forms)\n  \"Define a fexpr with the given NAME, PARAMETER-TREE,\nENVIRONMENT-PARAMETER, and FORMS.\"\n  (list #'%%def (%%function-symbol name)\n        (%%list* #'vau parameter-tree environment-parameter forms)))\n\n(defmacro def (definiend-tree value . docstring?)\n  \"Match the DEFINIEND-TREE against the VALUE and place resulting\nbindings into the current environment.  The optional DOCSTRING is\ncurrently ignored.\"\n  (list #'%%def definiend-tree value))\n\n(defmacro defconstant (name value . docstring?)\n  \"Define a constant with the given NAME and VALUE.  This is mostly\nfor documentation purposes, as constants are still mutable.  The\noptional DOCSTRING is currently ignored.\"\n  (list #'def name value))\n\n(defmacro lambda (parameter-tree . forms)\n  \"Create an anonymous function with the given PARAMETER-TREE and FORMS.\"\n  (list #'%%wrap (%%list* #'vau parameter-tree #ignore forms)))\n\n(defmacro defun (name parameter-tree . forms)\n  \"Define a function with the given NAME, PARAMETER-TREE, and FORMS.\"\n  (list #'def (%%function-symbol name)\n        (%%list* #'lambda parameter-tree forms)))\n\n;;; Built-Ins\n\n(defmacro progn forms\n  \"Sequentially evaluate FORMS, returning the value of the last one,\nor void if there are no forms.\"\n  (list* #'%%progn forms))\n\n(defmacro if (test consequent alternative)\n  \"Evaluate the TEST which must yield a boolean.  Then evaluate either\nthe CONSEQUENT or ALTERNATIVE depending on whether the TEST yielded\ntrue or false.\"\n  (list #'%%if test consequent alternative))\n\n(defmacro catch (tag . forms)\n  \"Establish a catch tag and evaluate FORMS as an implicit `progn'\ninside it.  The forms may use `throw' to nonlocally exit from the\ntag.  Usually, `block' should be preferred.\"\n  (list #'%%catch tag (list* #'lambda () forms)))\n\n(defun throw (tag . result?)\n  \"Abort to a nesting catch tag established by `catch' and pass the\nRESULT to it.\"\n  (%%throw tag (optional result?)))\n\n(defmacro loop forms\n  \"Evaluate the FORMS in an infinite loop.\"\n  (list #'%%loop (list* #'progn forms)))\n\n(defun eq (a b)\n  \"Return true if the values A and B are pointer-identical, false otherwise.\"\n  (%%eq a b))\n\n(defun class-of (object)\n  \"Return the class of the OBJECT.\"\n  (%%class-of object))\n\n(defun typep (object class)\n  \"Return true if the OBJECT is an instance of the CLASS, false otherwise.\"\n  (%%typep object class))\n\n(defun intern (string)\n  \"Get or create the unique symbol with STRING as name.\"\n  (%%intern string))\n\n(defun symbol-name (symbol)\n  \"Return the name of the SYMBOL as a string.\"\n  (%%symbol-name symbol))\n\n(defun variable-symbol (symbol)\n  \"Return the symbol with the same name as SYMBOL, but in the variable namespace.\"\n  (%%variable-symbol symbol))\n\n(defun function-symbol (symbol)\n  \"Return the symbol with the same name as SYMBOL, but in the function namespace.\"\n  (%%function-symbol symbol))\n\n(defun class-symbol (symbol)\n  \"Return the symbol with the same name as SYMBOL, but in the class namespace.\"\n  (%%class-symbol symbol))\n\n(defun keyword-symbol (symbol)\n  \"Return the symbol with the same name as SYMBOL, but in the keyword namespace.\"\n  (%%keyword-symbol symbol))\n\n(defun cons (car cdr)\n  \"Create a cons with the given CAR and CDR.\"\n  (%%cons car cdr))\n\n(defun car (cons)\n  \"Return the contents of the address part of the register.\"\n  (%%car cons))\n\n(defun cdr (cons)\n  \"Return the contents of the decrement part of the register.\"\n  (%%cdr cons))\n\n(defun list* arguments\n  \"Create a list from the ARGUMENTS so that the last argument becomes\nthe `cdr' of the list.\"\n  (apply #'%%list* arguments))\n\n(defun reverse (list)\n  \"Reverse the LIST.\"\n  (%%reverse list))\n\n(defun wrap (operator)\n  \"Create a new function that wraps around an underlying OPERATOR, and\ninduces argument evaluation around it.\"\n  (%%wrap operator))\n\n(defun unwrap (function)\n  \"Return the underlying operator of a FUNCTION.\"\n  (%%unwrap function))\n\n(defun eval (form environment)\n  \"Evaluate the FORM in the ENVIRONMENT, returning its result.\"\n  (%%eval form environment))\n\n(defun make-environment parent-environment?\n  \"Create a new environment with an optional PARENT-ENVIRONMENT in\nwhich bindings are looked up if they are not found.\"\n  (apply #'%%make-environment parent-environment?))\n\n(defun boundp (symbol environment)\n  \"Return true if the SYMBOL is bound in the ENVIRONMENT, false otherwise.\"\n  (%%boundp symbol environment))\n\n(defun panic (error)\n  \"Signal the ERROR in such a way that it cannot be caught by\ncondition handlers or `unwind-protect'.  This unconditionally breaks\nout of Lisp.\"\n  (%%panic error))\n\n;;; Lexical Bindings\n\n(defmacro let (bindings . forms)\n  \"Establish BINDINGS parallelly during the evaluation of FORMS, so\nthat no binding can refer to the other ones.\n\n$(syntax binding (definiend-tree value))\"\n  (list* (list* #'lambda (mapcar #'car bindings)\n                forms)\n         (mapcar #'cadr bindings)))\n\n(defmacro let* (bindings . forms)\n  \"Establish BINDINGS serially during the evaluation of FORMS, so that\nevery binding can refer to previous ones.\n\n$(syntax binding (definiend-tree value))\"\n  (if (null bindings)\n      (list* #'let () forms) ; Always introduce a new scope.\n      (list #'let (list (car bindings))\n            (list* #'let* (cdr bindings) forms))))\n\n(defmacro %letrec (bindings . forms)\n  \"Utility to establish BINDINGS recursively during the evaluation of\nFORMS.  Used by `labels'.\"\n  (if (null bindings)\n      (list* #'let () forms) ; Always introduce a new scope.\n      (list* #'let ()\n             (list #'def\n                   (mapcar #'car bindings)\n                   (list* #'list (mapcar #'cadr bindings)))\n             forms)))\n\n(defun %function-binding ((name parameter-tree . forms))\n  \"Utility to turn a function binding as it appears in `flet' and\n`labels' into a binding for `let' or `%letrec'.\"\n  (list (function-symbol name) (list* #'lambda parameter-tree forms)))\n\n(defmacro flet (function-bindings . forms)\n  \"Establish FUNCTION-BINDINGS parallelly during evaluation of FORMS,\nso that no function can refer to the other ones.\n\n$(syntax function-binding (name parameter-tree . forms))\"\n  (list* #'let (mapcar #'%function-binding function-bindings) forms))\n\n(defmacro labels (function-bindings . forms)\n  \"Establish FUNCTION-BINDINGS recursively during evaluation of FORMS,\nso that every function can refer to the other ones.\n\n$(syntax function-binding (name parameter-tree . forms))\"\n  (list* #'%letrec (mapcar #'%function-binding function-bindings) forms))\n\n;;; Data and Control Flow\n\n(defexpr quote (operand) #ignore\n  \"Return the unevaluated OPERAND.\"\n  operand)\n\n(defexpr the-environment () environment\n  \"Return the current environment.\"\n  environment)\n\n(defun apply (function arguments)\n  \"Call the FUNCTION with a dynamically-supplied list of ARGUMENTS.\"\n  (eval (cons (unwrap function) arguments) (%%make-environment)))\n\n(defmacro when (test . forms)\n  \"If TEST yields true, evaluate the FORMS as an implicit `progn'.\nOtherwise, return void.\"\n  (list #'if test (list* #'progn forms) #void))\n\n(defmacro unless (test . forms)\n  \"If TEST yields false, evaluate the FORMS as an implicit `progn'.\nOtherwise, return void.\"\n  (list #'if test #void (list* #'progn forms)))\n\n(defexpr cond clauses env\n  \"Multi-armed conditional.\n\nGo through the CLAUSES in order.  Evaluate the TEST.  If it yields\ntrue, evaluate the FORMS as an implicit `progn'.  If it yields false,\ngo to the next clause, or return void if there are no more clauses.\n\n$(syntax clause (test . forms))\"\n  (unless (null clauses)\n    (let ((((test . forms) . rest-clauses) clauses))\n      (if (eval test env)\n          (eval (cons #'progn forms) env)\n          (eval (cons #'cond rest-clauses) env)))))\n\n(defun not (boolean)\n  \"Invert the BOOLEAN.\"\n  (if boolean #f #t))\n\n(defexpr and operands env\n  \"Return true if all OPERANDS evaluate to true, false otherwise.  If\nan operand evaluates to false, later operands are not evaluated.  If\nthere are no operands, return false.\"\n  (cond ((null operands)           #t)\n        ((null (cdr operands))     (the boolean (eval (car operands) env)))\n        ((eval (car operands) env) (eval (cons #'and (cdr operands)) env))\n        (#t                        #f)))\n\n(defexpr or operands env\n  \"Return true if one of the OPERANDS evaluates to true, false\notherwise.  If an operand evaluates to true, later operands are not\nevaluated.  If there are no operands, return true.\"\n  (cond ((null operands)           #f)\n        ((null (cdr operands))     (the boolean (eval (car operands) env)))\n        ((eval (car operands) env) #t)\n        (#t                        (eval (cons #'or (cdr operands)) env))))\n\n(defexpr while (test-form . forms) env\n  \"Evaluate FORMS while TEST-FORM evaluates to true.\"\n  (let ((forms (list* #'progn forms)))\n    (block exit\n      (loop\n        (if (eval test-form env)\n            (eval forms env)\n            (return-from exit))))))\n\n(defmacro until (test-form . forms)\n  \"Evaluate FORMS until TEST-FORM evaluates to true.\"\n  (list* #'while (list #'not test-form) forms))\n\n(defmacro dotimes ((var count-form . result-form?) . body-forms)\n  \"Cf. Common Lisp's DOTIMES.\"\n  (flet ((dotimes* (n #'body #'result)\n           (let ((#'i (box 0)))\n             (while (< (i) n)\n               (body (i))\n               (i (+ (i) 1)))\n             (result (i)))))\n    (list #'dotimes*\n          count-form\n          (list* #'lambda (list var) body-forms)\n          (list* #'lambda (list var) result-form?))))\n\n(defmacro loop-let (name initializers . forms)\n  \"Labelled recursive loop, analogous to Scheme's named `let'.\n\nLexically bind a function named NAME with one PARAMETER for every\nINITIALIZER and the FORMS as body.  Then immediately apply the\nfunction to a list containing one VALUE for every INITIALIZER and\nreturn the result.  The function is bound per `labels' so it can\nrecursively refer to itself.\n\n$(syntax initializer (parameter value))\"\n  (list #'labels (list (list* name (mapcar #'car initializers) forms))\n        (list* name (mapcar #'cadr initializers))))\n\n(defexpr block (block-name . forms) env\n  \"Establish a block named BLOCK-NAME and evaluate the FORMS as an\nimplicit `progn' inside it.  The forms may use `return-from' to\nnonlocally exit from the block.\n\nNote that unlike in Common Lisp, there is no separate namespace for\nblock names; a block is named in the normal variable namespace.\"\n  (let ((tag (list #void))) ; cons up a fresh object as tag\n    (flet ((escape (value) (throw tag value)))\n      (catch tag\n        (eval (list (list* #'lambda (list block-name) forms)\n                    #'escape)\n              env)))))\n\n(defun return-from (#'block-name . value?)\n  \"Abort evaluation and return the optional VALUE (which defaults to\nvoid) from the block named BLOCK-NAME.  It is an error to return from\na block whose dynamic extent has ended.\n\n$(fn (block-name . value?))\"\n  (block-name (optional value?)))\n\n(defmacro unwind-protect (protected-form . cleanup-forms)\n  \"Evaluate the PROTECTED-FORM and return its result.  Regardless of\nwhether the protected form returns normally, or via a nonlocal exit,\nthe CLEANUP-FORMS are evaluated after the protected form.  Note that\nthe protected forms are not evaluated in case of `panic'.\"\n  (list #'%%unwind-protect protected-form (list* #'progn cleanup-forms)))\n\n(defexpr set (environment definiend-tree value) dynamic-environment\n  \"Match the DEFINIEND-TREE against the VALUE in the ENVIRONMENT,\ncreating or updating existing bindings.\"\n  (eval (list #'def definiend-tree\n              (list (unwrap #'eval) value dynamic-environment))\n        (eval environment dynamic-environment)))\n\n(defexpr prog1 (form . forms) env\n  \"Evaluate FORM and any additional FORMS, and return the result of FORM.\"\n  (let ((result (eval form env)))\n    (eval (list* #'progn forms) env)\n    result))\n\n(defexpr typecase (keyform . clauses) env\n  \"Multi-armed type test.\n\nEvaluate the KEYFORM.  Go through the CLAUSES.  If the result of\nevaluating KEYFORM is an instance of the class named by CLASS-NAME,\nevaluate the FORMS as an implicit `progn'.  Otherwise go to the next\nclause, or return void if there are no more clauses.\n\n$(syntax clause (class-name . forms))\"\n  (let ((key (eval keyform env)))\n    (loop-let -typecase- ((clauses clauses))\n      (unless (null clauses)\n        (let ((((class-name . forms) . rest-clauses) clauses))\n          (if (typep key (find-class class-name env))\n              (eval (cons #'progn forms) env)\n              (-typecase- rest-clauses)))))))\n\n(defun assert (boolean)\n  \"Signal an error if the BOOLEAN is false.  Otherwise return void.\"\n  (unless boolean (error (make-instance (class assertion-error)))))\n\n;;; Lists\n\n(defun null (object)\n  \"Return true if the OBJECT is nil, false otherwise.\"\n  (eq object #nil))\n\n(defun caar (cons)\n  \"Return the `car' of the `car' of the CONS.\"\n  (car (car cons)))\n\n(defun cadr (cons)\n  \"Return the `car' of the `cdr' of the CONS.\"\n  (car (cdr cons)))\n\n(defun cdar (cons)\n  \"Return the `cdr' of the `car' of the CONS.\"\n  (cdr (car cons)))\n\n(defun cddr (cons)\n  \"Return the `cdr' of the `cdr' of the CONS.\"\n  (cdr (cdr cons)))\n\n(defun append (list1 list2)\n  \"Append two lists.  The first one must be proper and is copied.  The\nsecond one is not copied (and doesn't even have to be a list). It\nbecomes the `cdr' of the final cons of the first list, or is returned\ndirectly if the first list is empty.\"\n  (%%append list1 list2))\n\n(defun nth (n list)\n  \"Return element number N of LIST, where the `car' is element zero.\"\n  (%%nth n list))\n\n(defun nthcdr (n list)\n  \"Returns the tail of LIST that would be obtained by calling `cdr' N\ntimes in succession.\"\n  (%%nthcdr n list))\n\n(defun mapcar (#'function list)\n  \"Create a new list by applying the FUNCTION to every element of the LIST.\"\n  (if (null list)\n      #nil\n      (cons (function (car list)) (mapcar #'function (cdr list)))))\n\n(defun mapc (#'function list)\n  \"Apply the FUNCTION to every element of the LIST for effect.  Return the list.\"\n  (unless (null list)\n    (function (car list))\n    (mapc #'function (cdr list)))\n  list)\n\n(defmacro dolist ((var list-form . result-form?) . body-forms)\n  \"Cf. Common Lisp's DOLIST.\"\n  (labels ((dolist* (list #'body #'result)\n             (if (null list)\n                 (result list)\n                 (progn\n                   (body (car list))\n                   (dolist* (cdr list) #'body #'result)))))\n    (list #'dolist*\n          list-form\n          (list* #'lambda (list var) body-forms)\n          (list* #'lambda (list var) result-form?))))\n\n(defun reduce (#'function list :initial-value initial-value)\n  \"Use the binary FUNCTION to combine the elements of the LIST.  The\nINITIAL-VALUE is logically placed before the list.\"\n  (if (null list)\n      initial-value\n      (reduce #'function (cdr list) :initial-value (function initial-value (car list)))))\n\n(defun memberp (element list :test #'test)\n  \"Return true if the ELEMENT is contained in the LIST according to\nthe TEST predicate, false otherwise.\"\n  (if (null list)\n      #f\n      (if (test element (car list))\n          #t\n          (memberp element (cdr list) :test #'test))))\n\n(defun remove-if (#'test list)\n  \"Return a new list from which the elements that satisfy the TEST\nhave been removed.\"\n  (if (null list)\n      #nil\n      (if (test (car list))\n          (remove-if #'test (cdr list))\n          (cons (car list) (remove-if #'test (cdr list))))))\n\n(defun get? (plist indicator)\n  \"Search for the INDICATOR keyword in the property list PLIST (a list\nof alternating keywords and values) and return the found value as an\noption.\"\n  (if (null plist)\n      #nil\n      (let (((i v . plist) plist))\n        (if (eq i indicator)\n            (some v)\n            (get? plist indicator)))))\n\n;;; Relational Operators\n\n;; Note that unlike in Common Lisp, these operators currently require\n;; at least two arguments.  This will be improved in the future.\n\n(defun %relational-operator (#'binary-operator)\n  \"Utility to create an n-ary relational operator from a BINARY-OPERATOR.\"\n  (labels ((operator (arg1 arg2 . rest)\n             (if (binary-operator arg1 arg2)\n                 (if (null rest)\n                     #t\n                     (apply #'operator (list* arg2 rest)))\n                 #f)))\n    #'operator))\n\n(def #'= (%relational-operator #'%%=)\n  \"Return true if all ARGUMENTS are equal, false otherwise.\n\n$(fn arguments)\n$(type function)\")\n\n(def #'< (%relational-operator #'%%<)\n  \"Return true if the ARGUMENTS are in monotonically increasing order,\nfalse otherwise.\n\n$(fn arguments)\n$(type function)\")\n\n(def #'> (%relational-operator #'%%>)\n  \"Return true if the ARGUMENTS are in monotonically decreasing order,\nfalse otherwise.\n\n$(fn arguments)\n$(type function)\")\n\n(def #'<= (%relational-operator #'%%<=)\n  \"Return true if the ARGUMENTS are in monotonically nondecreasing\norder, false otherwise.\n\n$(fn arguments)\n$(type function)\")\n\n(def #'>= (%relational-operator #'%%>=)\n  \"Return true if the ARGUMENTS are in monotonically nonincreasing\norder, false otherwise.\n\n$(fn arguments)\n$(type function)\")\n\n(defun /= (arg . args)\n  \"Return true if no two ARGUMENTS are the same, false otherwise.\n\n$(fn arguments)\"\n  (if (null args)\n      #t\n      (if (memberp arg args :test #'=)\n          #f\n          (apply #'/= args))))\n\n;;; Numbers\n\n;; The terms thetic (for + and *) and lytic (for - and /) are due to Hankel.\n\n(defun %thetic-operator (#'binary-operator initial-value)\n  \"Utility to create an n-ary thetic operator from a BINARY-OPERATOR and INITIAL-VALUE.\"\n  (lambda args\n    (reduce #'binary-operator args :initial-value initial-value)))\n\n(def #'+ (%thetic-operator #'%%+ 0)\n  \"Return the sum of the ARGUMENTS, or 0 if no arguments are supplied.\n\n$(fn arguments)\n$(type function)\")\n\n(def #'* (%thetic-operator #'%%* 1)\n  \"Return the product of the ARGUMENTS, or 1 if no arguments are supplied.\n\n$(fn arguments)\n$(type function)\")\n\n(defun %lytic-operator (#'binary-operator initial-value)\n  \"Utility to create an n-ary lytic operator from a BINARY-OPERATOR and INITIAL-VALUE.\"\n  (lambda (arg1 . rest)\n    (if (null rest)\n        (binary-operator initial-value arg1)\n        (reduce #'binary-operator rest :initial-value arg1))))\n\n(def #'- (%lytic-operator #'%%- 0)\n  \"If only one number is supplied in the ARGUMENTS, return the\nnegation of that number. If more than one number is supplied, subtract\nall of the later ones from the first one and return the result.\n\n$(fn arguments)\n$(type function)\")\n\n(def #'/ (%lytic-operator #'%%/ 1)\n  \"If only one number is supplied in the ARGUMENTS, return the\nreciprocal of that number.  If more than one number is supplied,\ndivide the first one by all of the later ones and return the result.\n\n$(fn arguments)\n$(type function)\")\n\n;;; Classes\n\n(defmacro class (name)\n  \"Access a class by the (unevaluated) NAME symbol in the current\nenvironment.  This is required because classes have their own\nnamespace.\"\n  (class-symbol name))\n\n(defun find-class (name environment)\n  \"Look up a class based on its NAME symbol (evaluated) in the given ENVIRONMENT.\"\n  (eval (class-symbol name) environment))\n\n(defun class-name (class)\n  \"Return the name symbol of the CLASS.\"\n  (%%class-name class))\n\n(defun subclassp (class superclass)\n  \"Return true if the CLASS is a subclass of the SUPERCLASS, false otherwise.\nA class is considered a subclass of itself.\"\n  (%%subclassp class superclass))\n\n(defexpr defclass (name superclass? slot-specs . properties) env\n  \"Define a new `standard-class' with the given NAME, optional\nSUPERCLASS, and SLOT-SPECS.  The superclass defaults to\n`standard-object'.  The SLOT-SPECS and PROPERTIES are currently\nignored.\n\nNote: In the future, this will redefine the class if it already\nexists, but for now it always creates a new class.  Therefore it is\nbest to avoid multiple calls to `defclass' for the same class.\n\n$(syntax slot-spec symbol)\n$(syntax property (:documentation docstring))\"\n  (def superclass (find-class (optional superclass? 'standard-object) env))\n  ;; Slot-specs are ignored for now, but check them nevertheless.\n  (dolist (slot-spec slot-specs)\n    (the symbol slot-spec))\n  (eval (list #'def (class-symbol name) (%%make-standard-class name superclass)) env)\n  name)\n\n;;; Generic Functions\n\n(defexpr defgeneric (name (receiver . parameters) . properties) env\n  \"Define a new generic function with the given NAME.  The RECEIVER,\nPARAMETERS, and PROPERTIES are currently ignored.\n\n$(syntax property (:documentation docstring))\"\n  (def generic (lambda args (%%invoke-method name args)))\n  (eval (list #'def (function-symbol name) generic) env)\n  name)\n\n(defexpr defmethod (name ((receiver class-name) . parameters) . forms) env\n  \"Add a new method to the generic function named by NAME specialized\nfor the class named by CLASS-NAME.\"\n  (def class (find-class class-name env))\n  ;; Note that the method can be a fexpr because the generic function\n  ;; has already evaluated the arguments when it is called.\n  (def method (eval (list* #'vau (cons receiver parameters) #ignore forms) env))\n  (%%put-method class name method)\n  name)\n\n;;; Standard Objects\n\n(defun make-instance (class . slot-inits)\n  \"Create a new instance of CLASS (that must be a `standard-class').\nThe SLOT-INITS must be of even length, and alternately contain slot\nnames (symbols, typically keywords) and values.\"\n  (apply #'%%make-instance (cons class slot-inits)))\n\n(defun slot-value (object slot-name)\n  \"Return the value of the slot named SLOT-NAME of the OBJECT.\"\n  (%%slot-value object slot-name))\n\n(defun set-slot-value (object slot-name value)\n  \"Set the value of the slot named SLOT-NAME of the OBJECT to VALUE.\"\n  (%%set-slot-value object slot-name value))\n\n(defun slot-bound-p (object slot-name)\n  \"Return true if the slot named SLOT-NAME of the OBJECT is set, false otherwise.\"\n  (%%slot-bound-p object slot-name))\n\n;;; Type Checks\n\n(defun make-type-error (datum expected-type)\n  \"Create a `type-error' with the given DATUM and EXPECTED-TYPE.\"\n  (make-instance (class type-error) :datum datum :expected-type expected-type))\n\n(defun assert-type (object class)\n  \"Signal a `type-error' if the OBJECT is not an instance of the CLASS.\"\n  (if (typep object class)\n      object\n      (error (make-type-error object (class-name class)))))\n\n(defexpr the (class-name object) env\n  \"Shorthand for `assert-type'.  Signal a `type-error' if the OBJECT\nis not an instance of the class named by CLASS-NAME.\"\n  (assert-type (eval object env) (find-class class-name env)))\n\n;;; Sequences\n\n(defgeneric length (sequence)\n  (:documentation \"Return the number of elements in a sequence.\"))\n\n(defmethod length ((seq list))\n  (%%list-length seq))\n\n(defgeneric elt (sequence index)\n  (:documentation \"Return the sequence element at the specified index.\"))\n\n(defmethod elt ((seq list) index)\n  (nth index seq))\n\n(defgeneric subseq (sequence start . end?)\n  (:documentation \"Create a sequence that is a copy of the subsequence\nof the SEQUENCE bounded by START and optional END?.  If END?  is not\nsupplied or void, the subsequence stretches until the end of the\nlist.\"))\n\n(defmethod subseq ((seq list) start . end?)\n  (%%list-subseq seq start (optional end?)))\n\n(defmethod subseq ((seq string) start . end?)\n  (%%string-subseq seq start (optional end?)))\n\n;;; Options\n\n;; An option is either nil (\"none\"), or a one-element list (\"some\").\n;; Variables holding options are conventionally suffixed with \"?\".\n\n(defun some (value)\n  \"Create a one-element list from the VALUE.\"\n  (%%some value))\n\n(defexpr if-option ((name option?) then else) env\n  \"Destructure the OPTION.  If it's non-nil, evaluate the THEN form\nwith the NAME bound to the contents of the option.  If it's nil,\nevaluate the ELSE form.\"\n  ;; (Idea from Taylor R. Campbell's blag.)\n  (let ((o? (eval option? env)))\n    (if (null o?)\n        (eval else env)\n        (eval (list (list #'vau (list name) #ignore then)\n                    (car o?))\n              env))))\n\n(defmacro when-option ((name option?) . forms)\n  \"Destructure the OPTION.  If it's non-nil, evaluate the FORMS with\nthe NAME bound to the contents of the option.  If it's nil, return nil.\"\n  (list #'if-option (list name option?) (list* #'progn forms) #nil))\n\n(defmacro unless-option (option? . forms)\n  \"Destructure the OPTION.  If it's nil, evaluate the FORMS.  If it's\nnon-nil, return nil.\"\n  (list #'if-option (list #ignore option?) #nil (list* #'progn forms)))\n\n(defexpr optional (option? . default?) env\n  \"Return the contents of the OPTION?, or the DEFAULT? if the option\nis nil.  The default itself defaults to void.  The DEFAULT? is\nevaluated lazily, only when the OPTION? is nil.\"\n  (if-option (value (eval option? env))\n    value\n    (if-option (default default?)\n      (eval default env)\n      #void)))\n\n(defun optionals (list . defaults)\n  \"Similar to `optional', but provides DEFAULTS for any number of\nelements of LIST.\n\nCurrently, the DEFAULTS are always evaluated, but in the future, they\nmight be evaluated lazily, so they should not be used for effect.\"\n  (loop-let -optionals- ((list list) (defaults defaults))\n    (if (null list)\n        (if (null defaults)\n            #nil\n            (cons (car defaults) (-optionals- #nil (cdr defaults))))\n        (if (null defaults)\n            (cons (car list)     (-optionals- (cdr list) #nil))\n            (cons (car list)     (-optionals- (cdr list) (cdr defaults)))))))\n\n(defun get-option (option?)\n  \"Returns the contents of the option or signals an error if the\noption is nil.\"\n  (optional option? (simple-error \"Option is nil\")))\n\n;;; Boxes\n\n;; A box is a function that encapsulates a mutable value.\n\n(defun box initial-value?\n  \"Create a new box with the optional initial value.  Calling the\nbox without arguments returns the value.  Calling the box with an\nargument sets the value.\"\n  (def value (optional initial-value?))\n  (def env (the-environment))\n  (lambda new-value?\n    (if-option (new-value new-value?)\n      (set env value new-value)\n      value)))\n\n;;; Dynamic Binding\n\n(defmacro defdynamic (name value . docstring?)\n  \"Define a new dynamic variable with the given NAME and default VALUE.\nThe optional DOCSTRING is currently ignored.\"\n  (list #'def name (make-instance (class dynamic) :value value)))\n\n(defun dynamic (dynamic-variable)\n  \"Return the current value of the DYNAMIC-VARIABLE.\"\n  (slot-value dynamic-variable 'value))\n\n(defun set-dynamic (dynamic-variable value)\n  \"Set the current value of the DYNAMIC-VARIABLE.\"\n  (set-slot-value dynamic-variable 'value value))\n\n(defexpr dynamic-let (bindings . forms) env\n  \"Evaluate the FORMS with the dynamic variables specified by BINDINGS\ntemporarily bound to new values.  Bindings are established parallely\nas per `let'.\n\n$(syntax binding (dynamic-variable value))\"\n  (let ((dynamics (mapcar (lambda ((name #ignore)) (eval name env)) bindings))\n        (values (mapcar (lambda ((#ignore value)) (eval value env)) bindings))\n        (thunk (eval (list* #'lambda () forms) env)))\n    (%%progv dynamics values thunk)))\n\n(defmacro dynamic-let* (bindings . forms)\n  \"Evaluate the FORMS with the dynamic variables specified by BINDINGS\ntemporarily bound to new values.  Bindings are established serially as\nper `let*'.\"\n  (if (null bindings)\n      (list* #'progn forms)\n      (list #'dynamic-let (list (car bindings))\n            (list* #'dynamic-let* (cdr bindings) forms))))\n\n(defmacro progv (dynamic-variables values . forms)\n  \"Evaluate the FORMS with the list of DYNAMIC-VARIABLES temporarily\nbound to new VALUES.  The DYNAMIC-VARIABLES and VALUES lists must have\nthe same length.\"\n  (list #'%%progv dynamic-variables values (list* #'lambda () forms)))\n\n;;; Delimited Control Operators\n\n;; These operators follow the API put forth in the delimcc library\n;; at URL `http://okmij.org/ftp/continuations/implementations.html'.\n\n(defmacro push-prompt (prompt . forms)\n  \"Push the PROMPT and evaluate the FORMS inside the prompt.  This\ndelimits the continuation.\"\n  (list #'%%push-prompt prompt (list* #'lambda () forms)))\n\n(defmacro take-subcont (prompt name . forms)\n  \"Abort outwards to the PROMPT.  When the prompt is reached, evaluate\nthe FORMS with NAME bound to the captured continuation (which does not\ninclude the prompt).\"\n  (list #'%%take-subcont prompt (list* #'lambda (list name) forms)))\n\n(defmacro push-delim-subcont (prompt continuation . forms)\n  \"Push the PROMPT and compose the previously captured CONTINUATION\ninside it.  The FORMS are then evaluated inside the new continuation.\"\n  (list #'%%push-delim-subcont prompt continuation (list* #'lambda () forms)))\n\n(defmacro push-subcont-barrier forms\n  \"Push a continuation barrier that prevents the FORMS from capturing\nany continuations to the outside.\"\n  (list #'%%push-subcont-barrier (list* #'lambda () forms)))\n\n(defconstant +default-prompt+ 'default-prompt\n  \"This prompt is used for general coroutine-like use of continuations.\")\n\n;;; Condition System\n\n;; Condition handling and restart handling share some similarities\n;; while also being quite different in other respects.\n;;\n;; The main similarities between condition and restart handling are:\n;;\n;; * Both condition and restart handlers are arranged in\n;;   dynamically-bound handler chains, consisting of individual\n;;   handler frames.  Each frame binds a number of handlers.  We use\n;;   two dynamic variables, `*condition-handler-frame?*' and\n;;   `*restart-handler-frame?*', to point at the innermost frame of\n;;   each chain.  Note that the variables hold options, as indicated\n;;   by the question mark.\n;;\n;; * Signalling a condition and invoking a restart are very similar\n;;   operations, in that a handler is looked up in the chain, and\n;;   then its handler function is invoked.\n;;\n;; The main differences:\n;;\n;; * Conditions are classes organized in a type hierarchy\n;;   (e.g. `type-error' as subtype of `error'), whereas restarts are\n;;   plain names (e.g. `abort' and `continue').\n;;\n;; * A condition handler function always receives only a single\n;;   argument, the condition, whereas a restart handler function\n;;   receives any number of arguments passed to `invoke-restart'.\n;;\n;; * A condition handler function may decline handling a condition by\n;;   returning normally instead of performing a nonlocal exit; this\n;;   causes the search for a handler to continue.  In contrast, if a\n;;   restart handler function returns normally, the restart is\n;;   considered handled, and its result value is returned from\n;;   `invoke-restart'.\n;;\n;; * A restart handler may optionally have an interactive function\n;;   that prompts the user for arguments when the restart is invoked\n;;   by `invoke-restart-interactively'.\n;;\n;; * A restart handler may optionally be associated with a list of\n;;   conditions, to tell apart restarts belonging to different,\n;;   concurrently signalled conditions.\n;;\n;; We follow the Common Lisp condition system quite closely\n;; (including details like the condition firewall), with two\n;; differences:\n;;\n;; 1) There is no `with-condition-restarts'.  Instead there is an\n;;    additional keyword, `:associated-conditions', in the\n;;    handler-specs of `restart-bind' and `restart-case' that\n;;    establishes the associations.  Alternatively, `signal' and\n;;    `error' also support the establishment of restart handlers\n;;    associated with the signalled condition.\n;;\n;; 2) Every restart must have a non-nil name; anonymous restarts\n;;    are not supported.\n\n(defclass handler-frame ()\n  (handlers\n   parent-frame?)\n  (:documentation \"Instances of this class make up the condition and\nrestart handler chains.  Each frame stores a list of HANDLERS and an\noptional PARENT-FRAME.\"))\n\n(defclass condition-handler ()\n  (condition-class\n   handler-function)\n  (:documentation \"A condition handler is handling a particular\nCONDITION-CLASS (can be `object' to handle all conditions).  The\nHANDLER-FUNCTION receives a signalled condition as its single\nargument.\"))\n\n(defclass restart-handler ()\n  (restart-name\n   handler-function\n   interactive-function?\n   associated-conditions)\n  (:documentation \"A restart handler is handling a particular\nRESTART-NAME.  The HANDLER-FUNCTION receives the arguments passed to\n`invoke-restart'.  The optional INTERACTIVE-FUNCTION is called by\n`invoke-restart-interactively' and should prompt the user for required\narguments.  The ASSOCIATED-CONDITIONS are a list of conditions with\nwhich this handler is associated.  If the list is empty, the handler\nis applicable to any condition.  If it's not empty, the handler is\napplicable only to conditions in the list.\"))\n\n(defdynamic *condition-handler-frame?* #nil\n  \"An option holding the innermost condition handler frame.\")\n\n(defdynamic *restart-handler-frame?* #nil\n  \"An option holding the innermost restart handler frame.\")\n\n(defun %make-handler-bind-operator (#'handler-spec-parser handler-frame-dynamic)\n  \"Metaprogramming utility to create `handler-bind' and `restart-bind'.\n\nIt is parameterized by a function that takes apart the handler\nspecifications of the `handler-bind' and `restart-bind' forms and\nproduces handlers from them, as well as the dynamic variable holding\nthe handler chain (the variable itself as a first class object, not\nits value, so it can be used with `progv').\"\n  (vau (handler-specs . forms) env\n    (let ((handler-frame (make-instance\n                          (class handler-frame)\n                          :handlers (mapcar (lambda (spec)\n                                              (handler-spec-parser spec env))\n                                            handler-specs)\n                          :parent-frame? (dynamic handler-frame-dynamic))))\n      (progv (list handler-frame-dynamic) (list (some handler-frame))\n        (eval (list* #'progn forms) env)))))\n\n(def #'handler-bind\n  (%make-handler-bind-operator\n   (lambda ((class-name function-form) env)\n     (make-instance (class condition-handler)\n                    :condition-class\n                    (the class (find-class class-name env))\n                    :handler-function\n                    (the function (eval function-form env))))\n   *condition-handler-frame?*)\n\n  \"Establish condition handlers specified by HANDLER-SPECS around FORMS.\n\n$(fn (handler-specs . forms))\n$(syntax handler-spec (condition-class handler-function))\")\n\n(def #'restart-bind\n  (%make-handler-bind-operator\n   (lambda ((restart-name function-form . properties) env)\n     (make-instance (class restart-handler)\n                    :restart-name\n                    (the symbol restart-name)\n                    :handler-function\n                    (the function (eval function-form env))\n                    :interactive-function?\n                    (when-option (i-f-form (get? properties :interactive-function))\n                      (some (the function (eval i-f-form env))))\n                    :associated-conditions\n                    (when-option (a-cs-form (get? properties :associated-conditions))\n                      (the list (eval a-cs-form env)))))\n   *restart-handler-frame?*)\n\n  \"Establish restart handlers specified by HANDLER-SPECS around FORMS.\nYou should usually prefer `restart-case'.\n\n$(fn (handler-specs . forms))\n$(syntax handler-spec (restart-name handler-function . properties))\n$(syntax properties (&key interactive-function associated-conditions))\")\n\n(defun %make-handler-case-operator (#'handler-bind-operator)\n  \"Metaprogramming utility to create `handler-case'/`restart-case'\nfrom `handler-bind'/`restart-bind'.\"\n  (vau (handler-specs . forms) env\n    (block exit\n      ((block nested\n         (eval (list #'handler-bind-operator\n                     (mapcar (lambda ((name function-form . properties))\n                               (list* name\n                                      (lambda args\n                                        (return-from nested\n                                                     (lambda ()\n                                                       (apply (eval function-form env) args))))\n                                      properties))\n                             handler-specs)\n                     (list #'return-from exit (list* #'progn forms)))\n               env))))))\n\n(def #'handler-case (%make-handler-case-operator #'handler-bind)\n  \"Like `handler-bind', but the stack is unwound before a handler function is called.\n\n$(fn (handler-specs . forms))\")\n\n(def #'restart-case (%make-handler-case-operator #'restart-bind)\n  \"Like `restart-bind', but the stack is unwound before a handler function is called.\n\n$(fn (handler-specs . forms))\")\n\n(defun %error (condition)\n  \"Utility to signal the CONDITION.  If the condition is unhandled,\ninvoke the debugger.  Therefore never returns normally.  See `error'.\"\n  (%signal condition)\n  (invoke-debugger condition))\n\n(defun invoke-debugger (condition)\n  \"Invoke the debugger, which currently just means panicking.\"\n  (panic condition))\n\n(defun %signal (condition)\n  \"Utility to signal the CONDITION.  If the signal is unhandled,\nreturn void.  See `signal'.\"\n  (loop-let -signal- ((handler-frame? (dynamic *condition-handler-frame?*)))\n    ;; Payload to `%find-handler?' is always nil for condition handlers.\n    (if-option ((handler frame) (%find-handler? condition handler-frame? #nil))\n      (progn\n        ;; Handler found; call it, passing along frame.\n        (%call-condition-handler handler frame condition)\n        ;; Signal unhandled: continue search for handlers.\n        (-signal- (slot-value frame 'parent-frame?)))\n      ;; No handler found, return void.\n      #void)))\n\n(defun %make-signal-with-restarts-operator (#'signal-operator)\n  \"Metaprogramming utility to create the `signal'/`error' operators\nthat take restart handler-specs from the `%signal'/`%error' ones that\ndon't.\"\n  (vau (condition . handler-specs) env\n    (let ((c (eval condition env)))\n      (eval (list #'restart-case (mapcar (lambda (handler-spec)\n                                           ;; Would be preferable to\n                                           ;; adjoin `c' to the set of\n                                           ;; any existing associated\n                                           ;; conditions, then we\n                                           ;; wouldn't need the rule\n                                           ;; against supplying\n                                           ;; `:associated-conditions'\n                                           ;; to `signal'/`error'.\n                                           (append\n                                            handler-spec\n                                            (list :associated-conditions (list #'list c))))\n                                         handler-specs)\n                  (list #'signal-operator c))\n            env))))\n\n(def #'signal (%make-signal-with-restarts-operator #'%signal)\n  \"Signal the CONDITION.  If the signal is unhandled, return void.\n\nRestart handlers that are associated with the condition can be bound\nas per `restart-case'.  The handlers should not specify the\n`:associated-conditions' property, as it will be set automatically.\n\n$(fn (condition . handler-specs))\")\n\n(def #'error (%make-signal-with-restarts-operator #'%error)\n  \"Signal the CONDITION.  If the condition is unhandled, invoke the\ndebugger.  Therefore never returns normally.\n\nRestart handlers that are associated with the condition can be bound\nas per `restart-case'.  The handlers should not specify the\n`:associated-conditions' property, as it will be set automatically.\n\n$(fn (condition . handler-specs))\")\n\n(defun %call-condition-handler (handler handler-frame condition)\n  \"Call a condition HANDLER's handler function with the given\nCONDITION.  During the call, the condition handler chain gets swapped\nto that chain that was active at the time the handler was established.\nThis is the so-called \\\"condition firewall\\\".  The chain gets passed\nin as the value of HANDLER-FRAME.\"\n  (dynamic-let ((*condition-handler-frame?* (slot-value handler-frame 'parent-frame?)))\n    (%apply-handler-function handler (list condition))))\n\n(defun %apply-handler-function (handler arguments)\n  \"Utility to call a condition or restart HANDLER's handler function\nwith a list of ARGUMENTS.\"\n  (apply (slot-value handler 'handler-function) arguments))\n\n(defun invoke-restart (restart-designator . arguments)\n  \"Invoke the restart designated by RESTART-DESIGNATOR, which can be a\nsymbol or a `restart-handler', with the given ARGUMENTS.  Signal an\nerror if the restart is not found.\"\n  (%invoke-restart-with-arguments-producing-function\n   restart-designator\n   (lambda (#ignore) arguments)))\n\n(defun invoke-restart-interactively (restart-designator)\n  \"Invoke the restart designated by RESTART-DESIGNATOR, which can be a\nsymbol or a `restart-handler', by prompting the user for arguments via\nthe restart's optional interactive function.  Signal an error if the\nrestart is not found.\"\n  (%invoke-restart-with-arguments-producing-function\n   restart-designator\n   (lambda (restart-handler)\n     (when-option (#'i-f (slot-value restart-handler 'interactive-function?))\n       (i-f)))))\n\n(defun %invoke-restart-with-arguments-producing-function (restart-designator #'function)\n  \"Utility to invoke the restart designated by RESTART-DESIGNATOR,\nwhich can be a symbol or a `restart-handler', with an arguments list\nproduced by FUNCTION (which receives a `restart-handler' as argument).\"\n  (typecase restart-designator\n    (symbol\n     (if-option (restart-handler (find-restart? restart-designator))\n       (%apply-handler-function restart-handler (function restart-handler))\n       (error (make-restart-error restart-designator))))\n    (restart-handler\n     (%apply-handler-function restart-designator (function restart-designator)))\n    (object\n     (error (make-type-error restart-designator '(or symbol restart-handler))))))\n\n(defun %find-handler? (object handler-frame? payload?)\n  \"Utility to find both condition handlers and restart handlers.\n\nThe OBJECT can be either a condition or a restart name.  The\nHANDLER-FRAME is the handler frame where the search should start\n(always the innermost handler frame at the start of the search).\n\nReturn an option of the found handler and the frame establishing it as\na two-element list.  The frame is needed so that we can access its\nparent in the implementation of the condition firewall (see\n`signal' and `%call-condition-handler').\n\nThe PAYLOAD? parameter can be used to pass in an optional condition if\nwe are looking for a restart handler (see `find-restart?').  If we are\nlooking for a condition handler, it is always nil.\"\n  (when-option (handler-frame handler-frame?)\n    (block found\n      (dolist (handler (slot-value handler-frame 'handlers))\n        (when (%handler-applicable-p handler object payload?)\n          (return-from found (some (list handler handler-frame)))))\n      (%find-handler? object (slot-value handler-frame 'parent-frame?) payload?))))\n\n(defun find-restart? (name . condition?)\n  \"Find a restart handler by NAME, optionally limited to restarts\nassociated with a particular CONDITION.\"\n  (when-option ((handler #ignore) (%find-handler?\n                                   name\n                                   (dynamic *restart-handler-frame?*)\n                                   condition?))\n    (some handler)))\n\n(defgeneric %handler-applicable-p (handler object payload?)\n  (:documentation \"Return true if a condition or restart HANDLER is\napplicable, false otherwise.  The OBJECT can be a condition or a\nrestart name.  The PAYLOAD? is only used for restart handlers, and\nalways nil for condition handlers.\"))\n\n(defmethod %handler-applicable-p ((handler condition-handler) condition #nil)\n  \"A condition handler is applicable if the condition is an instance\nof its condition class.\"\n  (typep condition (slot-value handler 'condition-class)))\n\n(defmethod %handler-applicable-p ((handler restart-handler) restart-name condition?)\n  \"A restart handler is applicable to a restart name and optional condition...\"\n  ;; ...if the restart name matches the handler's restart name, and...\n  (and (eq restart-name (slot-value handler 'restart-name))\n       ;; ...the handler is applicable to the condition.\n       (%restart-handler-applicable-to-condition-p handler condition?)))\n\n(defun %restart-handler-applicable-to-condition-p (handler condition?)\n  \"A restart handler is applicable to an optional condition...\"\n  (if-option (condition condition?)\n    ;; ...if we are looking for restarts associated with a\n    ;; particular condition...\n    (let ((a-cs (slot-value handler 'associated-conditions)))\n      (if (null a-cs)\n          ;; ...if the restart handler is not associated with\n          ;; particular conditions,...\n          #t\n          ;; ...or if the condition we are looking is one of the\n          ;; handler's associated conditions.\n          (memberp condition a-cs :test #'eq)))\n    ;; ...if we are not looking for restarts associated with a\n    ;; particular condition then every handler is applicable.\n    #t))\n\n(defun compute-restarts condition?\n  \"Return the list of currently active restarts, with most recently\nestablished ones first, optionally limited to those that are\nexplicitly associated with the supplied CONDITION or not associated\nwith any condition.\"\n  (loop-let -compute-restarts- ((restarts '())\n                                (handler-frame? (dynamic *restart-handler-frame?*)))\n    (if-option (handler-frame handler-frame?)\n      (-compute-restarts- (append restarts\n                                  (remove-if\n                                   (lambda (restart)\n                                     (not (%restart-handler-applicable-to-condition-p\n                                           restart condition?)))\n                                   (slot-value handler-frame 'handlers)))\n                          (slot-value handler-frame 'parent-frame?))\n      restarts)))\n\n(defclass restart-error (error)\n  (restart-name)\n  (:documentation \"Signalled when no handler for RESTART-NAME is found.\"))\n\n(defun make-restart-error (restart-name)\n  \"Create a new `restart-error' for the given RESTART-NAME.\"\n  (make-instance (class restart-error) :restart-name restart-name))\n\n(defclass simple-error (error)\n  (message)\n  (:documentation \"Class for simple errors with a MESSAGE.\"))\n\n(defun make-simple-error (message)\n  \"Create a new simple error with a MESSAGE.\"\n  (make-instance (class simple-error) :message message))\n\n(defun simple-error (message)\n  \"Signal a simple error with a MESSAGE.\"\n  (error (make-simple-error message)))\n";

/***/ }),

/***/ "./src/js.lispx":
/*!**********************!*\
  !*** ./src/js.lispx ***!
  \**********************/
/***/ ((module) => {

module.exports = ";;; LispX JavaScript Interface\n\n;; Copyright (c) 2021, 2022 Manuel J. Simoni\n\n(defun js-eq (a b)\n  \"Return true if the values A and B are equal per JavaScript's strict\nequality, false otherwise.\"\n  (eq a b))\n\n(defun to-lisp-boolean (js-boolean)\n  \"Convert the JS-BOOLEAN to a Lisp boolean.\"\n  (%%to-lisp-boolean js-boolean))\n\n(defun to-js-boolean (lisp-boolean)\n  \"Convert the LISP-BOOLEAN to a JS boolean.\"\n  (%%to-js-boolean lisp-boolean))\n\n(defun to-lisp-number (js-number)\n  \"Convert the JS-NUMBER to a Lisp number.\"\n  (%%to-lisp-number js-number))\n\n(defun to-js-number (lisp-number)\n  \"Convert the LISP-NUMBER to a JS number.\"\n  (%%to-js-number lisp-number))\n\n(defun to-lisp-string (js-string)\n  \"Convert the JS-STRING to a Lisp string.\"\n  (%%to-lisp-string js-string))\n\n(defun to-js-string (lisp-string)\n  \"Convert the LISP-STRING to a JS string.\"\n  (%%to-js-string lisp-string))\n\n(defun to-lisp-function (js-function)\n  \"Convert the JS-FUNCTION to a Lisp function.\"\n  (%%to-lisp-function js-function))\n\n(defun to-js-function (lisp-operator)\n  \"Convert the LISP-OPERATOR to a JS function.\"\n  (%%to-js-function lisp-operator))\n\n(defun apply-js-function (js-function arguments)\n  \"Call JS-FUNCTION with a list of ARGUMENTS.\"\n  (apply (to-lisp-function js-function) arguments))\n\n(defun call-js-function (js-function . arguments)\n  \"Call JS-FUNCTION with the rest ARGUMENTS.\"\n  (apply-js-function js-function arguments))\n\n(defmacro js-lambda (parameter-tree . forms)\n  \"Construct a lambda with the given PARAMETER-TREE and body FORMS\nthat's callable from JS.\"\n  (list #'to-js-function\n        (list #'lambda parameter-tree\n              ;; Push a barrier around the body forms to prevent\n              ;; continuations from escaping to JS.\n              (list* #'push-subcont-barrier\n                     forms))))\n\n(defun js-global (name)\n  \"Access a JS global by NAME (a string).  Return undefined if the\nglobal doesn't exist.\"\n  (%%js-global name))\n\n(defun js-new (constructor . arguments)\n  \"Call the JS CONSTRUCTOR function with ARGUMENTS.\"\n  (apply #'%%js-new (cons constructor arguments)))\n\n(defun js-get (object name)\n  \"Access a property of a JS object by NAME (a string).\"\n  (%%js-get object name))\n\n(defun list-to-js-array (list)\n  \"Turn a list into a JS array.\"\n  (%%list-to-js-array list))\n\n(defun js-array-to-list (array)\n  \"Turn a JS array into a list.\"\n  (%%js-array-to-list array))\n\n(defun js-array elements\n  \"Create a new JS array from the given elements.\"\n  (list-to-js-array elements))\n\n(defmethod elt ((seq object) index)\n  \"`elt' for JS arrays.  We must put the method on `object' because we\ndon't have a more precise type for them.\"\n  (if (or (< index 0) (>= index (length seq)))\n      (error (make-instance (class out-of-bounds-error)))\n      (%%js-elt seq index)))\n\n(defmethod length ((seq object))\n  \"`length' for JS arrays.\"\n  (to-lisp-number (js-get seq \"length\")))\n\n(defun apply-js-method (receiver name arguments)\n  \"Invoke a JS method by NAME (a string) on the RECEIVER object,\npassing along the list of ARGUMENTS.\"\n  (%%apply-js-method receiver name arguments))\n\n(defun call-js-method (receiver name . arguments)\n  \"Invoke a JS method by NAME (a string) on the RECEIVER object,\npassing along the rest ARGUMENTS.\"\n  (apply #'apply-js-method (list receiver name arguments)))\n\n(defun js-method (method-name)\n  \"Create a function that when called will call the specified method.\"\n  (lambda (receiver . arguments)\n    (apply-js-method receiver method-name arguments)))\n\n(defmacro define-js-method (name method-name)\n  \"Define a new function with the given NAME (a symbol) that invokes a\nJS method named METHOD-NAME (a string).  The function takes one or\nmore arguments.  The first argument is the receiver of the method\ncall (\\\"this\\\"), the rest are the normal method arguments.\"\n  (list #'def (function-symbol name) (js-method method-name)))\n\n(defun js-undefined-option (value)\n  \"Turn a value that may be undefined into an option.\"\n  (if (eq value +js-undefined+)\n      #nil\n      (some value)))\n\n(defun js-null-option (value)\n  \"Turn a value that may be null into an option.\"\n  (if (eq value +js-null+)\n      #nil\n      (some value)))\n\n(defun await (promise)\n  \"Wait for the PROMISE to become fulfilled or rejected.\"\n  ;; Capture to the default prompt, and...\n  (take-subcont +default-prompt+ k\n    ;; ...return a new promise there.  From the caller's perspective,\n    ;; we are now paused -- but from context's perspective, we are\n    ;; returning a promise.  See `deftest' for how this nicely\n    ;; interacts with promise-based test frameworks.\n    (call-js-method promise \"then\"\n                    (js-lambda (value)\n                      ;; When the original promise is fulfilled with a\n                      ;; value, reinstate the continuation, returning\n                      ;; the value where `await' was called.\n                      (push-delim-subcont +default-prompt+ k value))\n                    (js-lambda (error)\n                      ;; When the original promise is rejected,\n                      ;; reinstate the continuation, signalling an\n                      ;; error where `await' was called.\n                      (push-delim-subcont +default-prompt+ k (error error))))))\n\n(defun sync (#'fun)\n  \"Create a function that will await an underlying function.\"\n  (lambda args (await (apply #'fun args))))\n\n(defmacro define-js-method/sync (name method-name)\n  \"Like `define-js-method', but awaits the method result.\"\n  (list #'def (function-symbol name) (sync (js-method method-name))))\n\n(defun sleep (ms)\n  \"Sleep for some milliseconds.\"\n  (await (%%sleep ms)))\n\n(defun js-log arguments\n  \"Log the ARGUMENTS to the JS console.\"\n  (apply #'%%js-log arguments))\n";

/***/ }),

/***/ "./src/print.lispx":
/*!*************************!*\
  !*** ./src/print.lispx ***!
  \*************************/
/***/ ((module) => {

module.exports = ";;; LispX Printer\n\n;; Copyright (c) 2021 Manuel J. Simoni\n\n(defun write (object . keywords)\n  \"Write OBJECT to STREAM (defaults to `*standard-output*').  Main\nprinter entry point.\n\n$(fn (object &key stream))\"\n  (%%write object (optional (get? keywords :stream) (dynamic *standard-output*))))\n\n(defun print1 (object)\n  \"Print OBJECT readably on the current line.\"\n  (dynamic-let ((*print-escape* #t))\n    (write object)))\n\n(defun uprint1 (object)\n  \"Print OBJECT unreadably on the current line.\"\n  (dynamic-let ((*print-escape* #f))\n    (write object)))\n\n(defun print (object)\n  \"Print OBJECT readably on a fresh line.\"\n  (fresh-line)\n  (print1 object))\n\n(defun uprint (object)\n  \"Print OBJECT unreadably on a fresh line.\"\n  (fresh-line)\n  (uprint1 object))\n";

/***/ }),

/***/ "./src/read.lispx":
/*!************************!*\
  !*** ./src/read.lispx ***!
  \************************/
/***/ ((module) => {

module.exports = ";;; LispX Reader\n\n;; Copyright (c) 2021 Manuel J. Simoni\n\n(defun read arguments\n  \"Reads an object from the STREAM (which defaults to\n`*standard-input*').  If EOF is reached, and `eof-error-p' is true\n(the default), `end-of-file' is signalled. If it is false, `eof-value'\nis returned (it defaults to void).\n$(fn (&optional stream eof-error-p eof-value))\"\n  (apply #'stream-read (optionals arguments (dynamic *standard-input*) #t #void)))\n\n;;; Unstable/Experimental API:\n\n(defgeneric stream-read (stream eof-error-p eof-value)\n  (:documentation \"Underlying, generic implementation of `read'.\nEvery stream class can provide a specialized method.\"))\n\n(defmethod stream-read ((stream input-stream) eof-error-p eof-value)\n  \"The default implementation of `stream-read' calls a built-in\nfunction written in JS.\"\n  (%%read stream eof-error-p eof-value))\n";

/***/ }),

/***/ "./src/stream.lispx":
/*!**************************!*\
  !*** ./src/stream.lispx ***!
  \**************************/
/***/ ((module) => {

module.exports = ";;; LispX Streams\n\n;; Copyright (c) 2021 Manuel J. Simoni\n\n;;; String Input Streams\n\n(defun make-string-input-stream (string)\n  \"Create a string input stream that reads from STRING.\"\n  (%%make-string-input-stream string))\n\n;;; String Output Streams\n\n(defun make-string-output-stream ()\n  \"Construct an empty string output stream.\"\n  (%%make-string-output-stream))\n\n(defun get-output-stream-string (stream)\n  \"Return the contents of the string output STREAM.\"\n  (%%get-output-stream-string stream))\n\n;;; Miscellaneous\n\n(defun fresh-line stream?\n  \"Ensure that the following output appears on a new line by itself.\nThe optional STREAM defaults to `*standard-output*'.\"\n  (%%fresh-line (optional stream? (dynamic *standard-output*))))\n";

/***/ }),

/***/ "./src/control.mjs":
/*!*************************!*\
  !*** ./src/control.mjs ***!
  \*************************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_control": () => (/* binding */ init_control)
/* harmony export */ });
/*
 * LispX Delimited Control
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds delimited control, delimited dynamic binding, and continuation
 * barriers to a virtual machine.
 *
 * The API follows the delimcc library:
 * `http://okmij.org/ftp/continuations/caml-shift-journal.pdf'
 * and the paper Delimited Dynamic Binding:
 * `http://okmij.org/ftp/papers/DDBinding.pdf'.
 *
 * Also implements continuation-aware first-order effects: sequencing,
 * looping, nonlocal exits, and unwind protection.
 *
 * ---
 *
 * We are implementing delimited continuations in the context of a
 * tree-walking interpreter.  The main idea of the implementation is
 * to run directly on the host language stack most of the time, and
 * only create on-heap Lisp continuations when they are captured.
 *
 * In the regular case, Lisp operators return their result as their
 * normal JS return value.  But when a capture is triggered, a
 * suspension helper object is returned instead.  This suspension
 * contains the desired prompt to which the capture should abort to,
 * as well as a user-supplied Lisp handler function that gets called
 * with the captured continuation once the prompt is reached.  The
 * suspension also contains the continuation frames captured so far.
 *
 * Every built-in Lisp operator must detect when one of its inner
 * sub-expressions returns a suspension instead of a regular result.
 * In this case, the operator must suspend itself also by adding any
 * required continuation frames to the suspension, and passing on the
 * suspension outwards to its caller.  This process ends when the
 * desired prompt is reached, at which point the user-supplied handler
 * is called.
 *
 * To reinstate a continuation, a resumption helper object is created.
 * Analogous to how a suspension gets passed outwards from the point
 * of continuation capture to the prompt, a resumption gets passed
 * inwards from the point where continuation composition is triggered.
 * A resumption contains the continuation frames that must be put back
 * from the heap onto the JS stack, as well as a user-supplied Lisp
 * handler function that is called inside the newly composed context
 * when all frames have been reinstated.
 *
 * Every built-in Lisp operator is written in such a way that it can
 * either be called normally, or during continuation resumption.  If
 * called normally, the operator will do its regular business.  If
 * called during resumption however, it will receive a resumption
 * helper object.  The operator will tell the resumption object to put
 * the remaining on-heap continuation frames stored in it back onto
 * the JS stack before proceeding.  This process ends when the
 * innermost continuation frame that originally triggered continuation
 * capture is reached, at which point the user-supplied handler is
 * called.
 */
function init_control(vm)
{
    /*** Continuations ***/

    /*
     * A continuation is organized as a stack of continuation frames.
     * The innermost frame corresponds to the %%TAKE-SUBCONT
     * expression that triggered continuation capture.  The outermost
     * frame corresponds to the expression that appeared directly
     * within the prompt-pushing expression (%%PUSH-PROMPT or
     * %%PUSH-DELIM-SUBCONT).  The prompt-pushing expression itself
     * is not included in the continuation.
     *
     * Every continuation frame contains a work function (a JavaScript
     * closure) that knows how to resume that particular frame when
     * the continuation is reinstated.  Every built-in Lisp operator
     * creates different kinds of work functions for the continuation
     * frames it creates.
     */
    vm.Continuation = class Lisp_continuation extends vm.Object
    {
        /*
         * Constructs a continuation frame with the operator-specific
         * work function and an inner continuation frame.
         *
         * The inner continuation frame is null for the innermost
         * frame created by the %%TAKE-SUBCONT expression that triggered
         * continuation capture.
         */
        constructor(work_fun, inner)
        {
            super();
            vm.assert_type(work_fun, "function");
            vm.assert_type(inner, vm.type_or(vm.TYPE_NULL, vm.Continuation));
            this.work_fun = work_fun;
            this.inner = inner;
        }
    };

    /*
     * A suspension is a helper object created during the capture
     * (creation) of a continuation.
     *
     * It gets passed outwards from the %%TAKE-SUBCONT expression that
     * triggers the capture until a %%PUSH-PROMPT or
     * %%PUSH-DELIM-SUBCONT with a matching prompt is reached.  Every
     * intervening Lisp expression adds one or more continuation
     * frames to the suspension on the way out.  Once the
     * %%PUSH-PROMPT or %%PUSH-DELIM-SUBCONT is reached, the
     * suspension's handler gets called with the captured
     * continuation.
     *
     * Suspensions are implementation-level objects, and are never
     * visible to Lisp.
     */
    vm.Suspension = class Suspension
    {
        /*
         * Constructs a new suspension.
         *
         * The continuation will be captured outwards to the prompt.
         *
         * The user-defined suspension handler will get called with
         * the captured continuation.
         */
        constructor(prompt, handler)
        {
            vm.assert_type(prompt, vm.TYPE_ANY);
            vm.assert_type(handler, vm.Function);
            this.prompt = prompt;
            this.handler = handler;
            this.continuation = null;
        }

        /*
         * Destructively adds a new outer continuation frame with the
         * given work function to the suspension as we move outwards
         * during continuation creation.
         */
        suspend(work_fun)
        {
            vm.assert_type(work_fun, "function");
            this.continuation = new vm.Continuation(work_fun, this.continuation);
            return this;
        }
    };

    /*
     * A resumption is a helper object created during the composition
     * (reinstatement) of a continuation.
     *
     * It gets passed inwards until the innermost continuation frame
     * is reached.  At each step, the operator-specific work function
     * of intervening frames is called.  Once the innermost frame is
     * reached, the user-defined resumption handler function gets
     * called and takes over control.
     *
     * Resumptions are implementation-level objects, and are never
     * visible to Lisp.
     */
    vm.Resumption = class Resumption
    {
        /*
         * Creates a new resumption.
         *
         * The continuation will get composed with the current stack,
         * frame by frame, as we move inwards.
         *
         * The user-defined resumption handler will get called after
         * the continuation has been reinstated.
         */
        constructor(continuation, handler)
        {
            vm.assert_type(continuation, vm.Continuation);
            vm.assert_type(handler, vm.Function);
            this.continuation = continuation;
            this.handler = handler;
        }

        /*
         * Destructively removes the outer frame of a continuation
         * from the resumption and calls its work function as we move
         * inwards during continuation reinstatement.
         */
        resume()
        {
            const continuation = this.continuation;
            this.continuation = continuation.inner;
            return continuation.work_fun(this);
        }
    };

    /*** Bind ***/

    /*
     * Sequences a thunk and a function in a continuation-aware
     * manner: the function receives the result of the thunk as its
     * argument.
     *
     * This is used in eval.mjs for all operators whose semantics are
     * straightforward and only require sequential execution.
     */
    vm.bind = (first, second) =>
    {
        vm.assert_type(first, "function");
        vm.assert_type(second, "function");
        return do_bind(first, second);
    };

    /*
     * Work function for bind().
     *
     * Note the resumption parameter.  Do_bind(), like all work
     * functions, is written so that it can be called in two ways:
     *
     * - Directly from bind(), with a null resumption.  In this case it
     *   will evaluate the first thunk.
     *
     * - As a work function of a suspended continuation frame.  This happens
     *   if the first thunk earlier captured a continuation.  In this case,
     *   we resume into the continuation with resume().
     *
     * Due to its simplicity, do_bind() is a good example to learn
     * about the protocol that continuation frames / work functions
     * must support.  The work functions of the more complicated
     * operators, below, follow this same protocol.
     */
    function do_bind(first, second, resumption = null)
    {
        /*
         * Evaluate first thunk.
         */
        let val;
        if (resumption instanceof vm.Resumption)
            /*
             * First thunk previously captured a continuation.  Resume
             * into it.
             */
            val = resumption.resume();
        else
            /*
             * We are evaluating the first thunk for the first time.
             */
            val = first();
        /*
         * Check result of first thunk.
         */
        if (val instanceof vm.Suspension)
            /*
             * The first thunk captured a continuation.
             *
             * We need to suspend now, too.  We do this by pushing a
             * work function closure onto the continuation,
             * that will restart later where we left off.
             */
            return val.suspend((resumption) =>
                do_bind(first, second, resumption));
        else
            /*
             * The first thunk returned normally.
             * Pass its result to the second function.
             */
            return second(val);
    }

    /*** Delimited Control Operators ***/

    /*
     * (%%take-subcont prompt handler) => |
     *
     * Built-in function that initiates continuation capture.  It
     * aborts outwards to the given prompt and calls the suspension
     * handler with the captured continuation.
     */
    vm.TAKE_SUBCONT = (args, env) =>
    {
        var prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        var handler = vm.assert_type(vm.elt(args, 1), vm.Function);

        /*
         * Create the suspension.
         *
         * This is the first event of continuation capture.
         */
        const suspension = new vm.Suspension(prompt, handler);

        /*
         * Push the innermost continuation frame onto the
         * suspension.
         *
         * Its work function will call the user-defined
         * resumption handler when resumed.
         */
        return suspension.suspend((resumption) =>
            /*
             * This is the final event of continuation composition.
             *
             * The resumption handler passed in from the outside takes
             * over control in the place where the continuation
             * was originally captured.
             */
            vm.operate(resumption.handler, vm.nil(), env)
        );
    };

    /*
     * (%%push-prompt prompt thunk) => result
     *
     * Built-in function that pushes a prompt.  A user-supplied thunk
     * is then called inside the newly delimited continuation.
     * Returns the thunk's result.
     */
    vm.PUSH_PROMPT = (args, env) =>
    {
        const prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const thunk = vm.assert_type(vm.elt(args, 1), vm.Function);
        const action = () => vm.operate(thunk, vm.nil(), env);
        return do_push_action(prompt, action, env);
    };

    /*
     * (%%push-delim-subcont prompt continuation thunk) => result
     *
     * Built-in function that pushes a prompt and reinstates a
     * previously captured continuation inside it.  A user-supplied
     * thunk is then called inside the newly composed continuation.
     * Returns the thunk's result.
     *
     * (Note: this operator is basically `push_delim_subcont' from
     * delimcc, except that the prompt must be manually supplied,
     * since our continuations don't include prompts.)
     */
    vm.PUSH_DELIM_SUBCONT = (args, env) =>
    {
        const prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const continuation = vm.assert_type(vm.elt(args, 1), vm.Continuation);
        const thunk = vm.assert_type(vm.elt(args, 2), vm.Function);
        /*
         * Resume into the user-supplied continuation with the thunk
         * as resumption handler.
         *
         * This is the first event of continuation composition.
         */
        const action = () => new vm.Resumption(continuation, thunk).resume();
        return do_push_action(prompt, action, env);
    };

    /*
     * Work function for PUSH_PROMPT and PUSH_DELIM_SUBCONT
     * whose difference is factored out into the action parameter.
     */
    function do_push_action(prompt, action, env, resumption = null)
    {
        /*
         * Do the action.
         */
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = action();

        if (result instanceof vm.Suspension) {
            /*
             * Action captured a continuation.
             */
            if (vm.equal(prompt, result.prompt)) {
                /*
                 * It's looking for our prompt, i.e. the capture ends here.
                 *
                 * This is the final event of continuation capture.
                 *
                 * The user-supplied suspension handler takes over
                 * control where the prompt was originally pushed.
                 * It receives the captured continuation as argument.
                 */
                return vm.operate(result.handler,
                                  vm.list(result.continuation),
                                  env);
            } else {
                /*
                 * It's looking for an outer prompt, we need to
                 * suspend, ourselves.
                 */
                return result.suspend((resumption) =>
                    do_push_action(prompt, action, env, resumption));
            }
        } else {
            /*
             * Action evaluated normally.
             */
            return result;
        }
    }

    /*
     * (%%push-subcont-barrier thunk) => result
     *
     * Built-in function that calls a thunk and prevents it from
     * capturing continuations to the outside.
     *
     * This is easily the hairiest part of the whole program.
     */
    vm.PUSH_SUBCONT_BARRIER = (args, env) =>
    {
        const thunk = vm.assert_type(vm.elt(args, 0), vm.Function);
        return do_push_subcont_barrier(thunk, env);
    };

    function do_push_subcont_barrier(thunk, env, resumption = null)
    {
        /*
         * How can it be that this work function must handle
         * resumption, you ask?  Isn't the whole idea behind a
         * continuation barrier that continuations cannot escape it,
         * and therefore obviously cannot reenter it either?  The
         * answer can be found in the next comment.
         */
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = vm.operate(thunk, vm.nil(), env);

        if (result instanceof vm.Suspension) {
            /*
             * Thunk attempted to capture.
             *
             * Add ourselves to the continuation.  Note that this
             * built-in is different from all others.  We do not
             * return the suspension back to the caller -- that is
             * after all exactly what we want to prevent.  But we must
             * still suspend ourselves in this way: if we didn't, the
             * barrier would be missing from the continuation.
             */
            result.suspend((resumption) =>
                do_push_subcont_barrier(thunk, env, resumption));

            /*
             * Here comes the klever part: resume back into the
             * continuation and throw an error from the inside.
             *
             * This means the user will be able to see a useful stack
             * trace that shows where the ill-fated continuation
             * capture occurred.  (This bold claim is yet to be
             * verified.)
             */
            const handler = vm.alien_function(() => {
                throw new vm.Prompt_not_found_error(result.prompt); });

            return new vm.Resumption(result.continuation, handler).resume();
        } else {
            return result;
        }
    };

    /*
     * Signalled on continuation barrier breach.
     */
    vm.Prompt_not_found_error = class Lisp_prompt_not_found_error extends vm.Error
    {
        constructor(prompt)
        {
            super("Prompt not found");
            this.lisp_slot_prompt = prompt;
        }
    };

    /*** Delimited Dynamic Binding ***/

    /*
     * A dynamic variable is a cell holding a value.
     */
    vm.Dynamic = class Lisp_dynamic extends vm.Standard_object
    {
        constructor(value = vm.void())
        {
            super();
            this.lisp_slot_value = value;
        }

        get_value() { return this.lisp_slot_value; }
        set_value(value) { this.lisp_slot_value = value; }
    };

    /*
     * Create a new dynamic variable with the given default value.
     */
    vm.make_dynamic = (value = vm.void()) =>
    {
        return new vm.Dynamic(value);
    };

    /*
     * (%%progv dynamics values thunk) => result
     *
     * Built-in function that evaluates a thunk with dynamic variables
     * temporarily bound to new values.
     *
     * Cf. Common Lisp's PROGV.
     */
    vm.PROGV = (args, env) =>
    {
        const dynamics = vm.list_to_array(vm.elt(args, 0));
        const values = vm.list_to_array(vm.elt(args, 1));
        const thunk = vm.assert_type(vm.elt(args, 2), vm.Function);
        return do_progv(dynamics, values, thunk, env);
    };

    function do_progv(dynamics, values, thunk, env, resumption = null)
    {
        return vm.progv(dynamics, values, () => {
            let result;
            if (resumption instanceof vm.Resumption) {
                result = resumption.resume();
            } else {
                result = vm.operate(thunk, vm.nil(), env);
            }
            if (result instanceof vm.Suspension) {
                return result.suspend((resumption) =>
                    do_progv(dynamics, values, thunk, env, resumption));
            } else {
                return result;
            }
        });
    }

    /*
     * Utility to bind dynamic variables during a JS thunk.
     *
     * This can also be used outside of the %%PROGV primitive.
     */
    vm.progv = (dynamics, values, thunk) =>
    {
        vm.assert(dynamics.length === values.length);
        /*
         * Save old values and apply new ones.
         */
        const old_values = [];
        for (let i = 0; i < dynamics.length; i++) {
            const dynamic = vm.assert_type(dynamics[i], vm.Dynamic);
            old_values[i] = dynamic.get_value();
            dynamic.set_value(values[i]);
        }
        try {
            /*
             * Call the thunk.
             */
            return thunk();
        } finally {
            /*
             * Restore old values.
             */
            for (let i = 0; i < dynamics.length; i++) {
                dynamics[i].set_value(old_values[i]);
            }
        }
    };

    /*** Simple Control ***/

    /*
     * (%%loop expr) => |
     *
     * Built-in operator that evaluates an expression in a never-ending cycle.
     *
     * Cf. Common Lisp's "simple" LOOP, NOT the Loop Facility.
     */
    vm.LOOP = (operands, env) =>
    {
        const expr = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        return do_loop(expr, env);
    };

    function do_loop(expr, env, resumption = null)
    {
        let first = true; // Only resume once.
        while (true) {
            let result;
            if (first && (resumption instanceof vm.Resumption)) {
                first = false;
                result = resumption.resume();
            } else {
                result = vm.eval(expr, env);
            }
            if (result instanceof vm.Suspension) {
                return result.suspend((resumption) =>
                    do_loop(expr, env, resumption));
            } else {
                continue;
            }
        }
    }

    /*
     * (%%catch tag thunk) => result
     *
     * Built-in function that calls a thunk with a catch tag.
     * Dynamically nested forms may nonlocally exit to the catch tag
     * with %%THROW.
     *
     * Cf. Common Lisp's CATCH.
     */
    vm.CATCH = (operands, env) =>
    {
        const tag = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        const thunk = vm.assert_type(vm.elt(operands, 1), vm.Function);
        return do_catch(tag, thunk, env);
    };

    function do_catch(tag, thunk, env, resumption = null)
    {
        try {
            let result;
            if (resumption instanceof vm.Resumption) {
                result = resumption.resume();
            } else {
                result = vm.operate(thunk, vm.nil(), env);
            }
            if (result instanceof vm.Suspension) {
                return result.suspend((resumption) =>
                    do_catch(tag, thunk, env, resumption));
            } else {
                return result;
            }
        } catch (e) {
            /*
             * Check if the exception we caught is a nonlocal exit
             * with our catch tag.
             *
             * If so, return its value.  Otherwise rethrow it.
             */
            if ((e instanceof vm.Nonlocal_exit) && (e.tag === tag)) {
                return e.value;
            } else {
                throw e;
            }
        }
    }

    /*
     * (%%throw tag value) => |
     *
     * Built-in function that nonlocally exits to the dynamically
     * nesting catch tag and passes the value to it.
     *
     * Cf. Common Lisp's THROW.
     */
    vm.THROW = (args, env) => {
        const tag = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const value = vm.assert_type(vm.elt(args, 1), vm.TYPE_ANY);
        throw new vm.Nonlocal_exit(tag, value);
    };

    /*
     * Instances of this class are thrown by %%THROW.
     */
    vm.Nonlocal_exit = class Nonlocal_exit
    {
        constructor(tag, value)
        {
            this.tag = tag;
            this.value = value;
        }
    };

    /*
     * (%%unwind-protect protected-expr cleanup-expr) => result
     *
     * Built-in operator that evaluates the protected expression and
     * returns its result.
     *
     * Regardless of whether the protected expression returns normally
     * or via a nonlocal exit, the cleanup expression is evaluated
     * (and its result discarded) after the protected expression.
     *
     * The cleanup expression is not evaluated when the protected
     * expression exits via a continuation capture or panic.  (A panic
     * is a special kind of exception whose purpose is to
     * unconditionally break out of Lisp and back into JS.)
     *
     * Cf. Common Lisp's UNWIND-PROTECT.
     */
    vm.UNWIND_PROTECT = (operands, env) =>
    {
        const protected_expr = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        const cleanup_expr = vm.assert_type(vm.elt(operands, 1), vm.TYPE_ANY);
        return do_unwind_protect_1(protected_expr, cleanup_expr, env);
    };

    /*
     * This must be implemented in two steps, with two work functions.
     *
     * The first one evaluates the protected expression, which may (a)
     * return normally, or (b) exit nonlocally with an exception, or (c)
     * capture a continuation, or (d) exit with a panic.
     *
     * If it does capture, we'll have to restart at step 1 later.  If
     * it does not capture, we can go to step 2, but have to remember
     * whether step 1 returned successfully or threw an exception.  If
     * it panics, we just call it quits, too.
     *
     * The second work function, step 2, evaluates the cleanup
     * expression and afterwards either returns the result produced by
     * the protected expression, or (re)throws the exception
     * thrown by it.
     */
    function do_unwind_protect_1(protected_expr, cleanup_expr, env, resumption = null)
    {
        try {
            let result;
            if (resumption instanceof vm.Resumption)
                result = resumption.resume();
            else
                result = vm.eval(protected_expr, env);
            if (result instanceof vm.Suspension)
                /*
                 * (c) Protected expression captured - stay at step 1.
                 */
                return result.suspend((resumption) =>
                    do_unwind_protect_1(protected_expr, cleanup_expr, env, resumption));
            else
                /*
                 * (a) Protected expression returned normally - go to step 2,
                 * remembering that step 1 was successful.
                 */
                return do_unwind_protect_2(cleanup_expr, result, true, env);
        } catch (exception) {
            /*
             * (d) Protected expression panicked.  Let the panic through.
             */
            if (exception instanceof vm.Panic)
                throw exception;
            else
                /*
                 * (b) Protected expression threw - go to step 2,
                 * remembering that step 1 failed.
                 */
                return do_unwind_protect_2(cleanup_expr, exception, false, env);
        }
    }

    function do_unwind_protect_2(cleanup_expr, value, success, env, resumption = null)
    {
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = vm.eval(cleanup_expr, env);
        if (result instanceof vm.Suspension) {
            return result.suspend((resumption) =>
                do_unwind_protect_2(cleanup_expr, value, success, env, resumption));
        } else {
            /*
             * After the cleanup expression has been evaluated:
             *
             * If the protected expression returned normally (a),
             * return its result now.
             *
             * If it threw an exception (b), rethrow the exception
             * now.
             */
            if (success)
                return value;
            else
                throw value;
        }
    }

    /*** Lisp API ***/

    vm.define_class("continuation", vm.Continuation);

    vm.define_class("dynamic", vm.Dynamic, vm.Standard_object, vm.Standard_class);

    vm.define_condition("prompt-not-found-error", vm.Prompt_not_found_error, vm.Error);

    vm.define_built_in_function("%%take-subcont", vm.TAKE_SUBCONT);

    vm.define_built_in_function("%%push-prompt", vm.PUSH_PROMPT);

    vm.define_built_in_function("%%push-delim-subcont", vm.PUSH_DELIM_SUBCONT);

    vm.define_built_in_function("%%push-subcont-barrier", vm.PUSH_SUBCONT_BARRIER);

    vm.define_built_in_function("%%progv", vm.PROGV);

    vm.define_built_in_operator("%%loop", vm.LOOP);

    vm.define_built_in_function("%%catch", vm.CATCH);

    vm.define_built_in_function("%%throw", vm.THROW);

    vm.define_built_in_operator("%%unwind-protect", vm.UNWIND_PROTECT);

};


/***/ }),

/***/ "./src/eval.mjs":
/*!**********************!*\
  !*** ./src/eval.mjs ***!
  \**********************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_eval": () => (/* binding */ init_eval)
/* harmony export */ });
/*
 * LispX Evaluation
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds evaluation-related functions and classes to a virtual machine.
 */
function init_eval(vm)
{
    /*** Evaluation & Operation Core ***/

    /*
     * Evaluate a form in an environment.
     *
     * If the form is neither a symbol nor a cons, it evaluates to
     * itself.
     *
     * The environment defaults to the VM's root environment.
     *
     * Prefer vm.eval_form() unless you are prepared to handle
     * suspensions.
     */
    vm.eval = (form, env = vm.get_environment()) =>
    {
        return vm.trap_exceptions(() => {
            vm.assert_type(env, vm.Environment);
            if (form instanceof vm.Symbol)
                return evaluate_symbol(form, env);
            else if (form instanceof vm.Cons)
                return evaluate_cons(form, env);
            else
                return form;
        });
    };

    /*
     * Evaluate a form, like vm.eval(), but throw an error if the
     * code captures a continuation.  This should usually be used
     * when calling Lisp from JavaScript.
     */
    vm.eval_form = (form, env = vm.get_environment()) =>
    {
        const result = vm.eval(form, env);
        if (result instanceof vm.Suspension)
            throw new vm.Prompt_not_found_error(result.prompt);
        else
            return result;
    };

    /*
     * Symbols evaluate to themselves if they are keywords, or to the
     * value they are bound to in the environment otherwise.
     */
    function evaluate_symbol(sym, env)
    {
        if (sym.get_namespace() === vm.KEYWORD_NAMESPACE)
            return sym;
        else
            return env.lookup(sym);
    }

    /*
     * Conses evaluate by first evaluating their car, which should
     * result in an operator.
     *
     * Then they tell the operator to operate on their cdr.
     */
    function evaluate_cons(cons, env)
    {
        // (See control.mjs for the definition of vm.bind().)
        return vm.bind(() => evaluate_operator(cons.car(), env),
                       (operator) => vm.operate(operator, cons.cdr(), env));
    }

    /*
     * Evaluate the operator position (car) of a cons.
     *
     * If it's a symbol, look it up in the function namespace.
     * This is the fundamental rule of Lisp-2 goodness.
     *
     * But if it's not a symbol, just evaluate it normally.
     * This gives us the same convenience as a Lisp-1.
     */
    function evaluate_operator(operator_form, env)
    {
        if (operator_form instanceof vm.Symbol)
            return env.lookup(operator_form.to_function_symbol());
        else
            return vm.eval(operator_form, env);
    }

    /*
     * Cause an operator to operate on an operand in the given
     * environment.
     *
     * The environment defaults to the VM's root environment.
     */
    vm.operate = (operator, operand, env = vm.get_environment()) =>
    {
        return vm.trap_exceptions(() => {
            vm.assert_type(operator, vm.Operator);
            vm.assert_type(operand, vm.TYPE_ANY);
            vm.assert_type(env, vm.Environment);
            return operator.operate(operand, env);
        });
    };

    /*** Definiends ***/

    /*
     * A definiend is what appears on the left-hand side of a
     * definition (DEF) or as a parameter in a fexpr or function.
     *
     * A plain definiend may either be a symbol or #IGNORE.
     *
     * A definiend tree allows arbitrary nesting of definiends.
     */
    const DEFINIEND = vm.type_or(vm.Symbol, vm.Ignore);
    const DEFINIEND_TREE = vm.type_or(vm.Symbol, vm.Ignore, vm.List);

    /*
     * Matches a definiend ("left-hand side") against a value
     * ("right-hand side") and places resulting bindings into the given
     * environment.  This is used everywhere names are bound to values,
     * such as in DEF, LET, fexpr and function parameters, etc.
     *
     * A non-keyword symbol as a definiend creates a new binding for
     * the whole value.
     *
     * A keyword symbol as a definiend requires the value to be equal
     * to itself, and does not produce a binding.  This yields a simple
     * mechanism for passing keyword arguments to functions.
     *
     * A cons definiend's car and cdr are treated as nested definiends
     * and recursively matched against the value's car and cdr, which
     * must be a cons, too.
     *
     * #NIL as a definiend requires the value to be #NIL, too.
     *
     * #IGNORE as a definiend simply ignores the value.
     *
     * Using any other object as a definiend signals an error.
     *
     * Returns the value.
     */
    vm.match = (definiend, value, env) =>
    {
        if (definiend instanceof vm.Symbol) {
            if (definiend.get_namespace() === vm.KEYWORD_NAMESPACE) {
                if (definiend !== value) {
                    throw new vm.Match_error(definiend, value);
                }
            } else {
                env.put(definiend, value);
            }
        } else if (definiend instanceof vm.Cons) {
            if (value instanceof vm.Cons) {
                vm.match(definiend.car(), value.car(), env);
                vm.match(definiend.cdr(), value.cdr(), env);
            } else {
                throw new vm.Match_error(definiend, value);
            }
        } else if (definiend === vm.nil()) {
            if (value !== vm.nil()) {
                throw new vm.Match_error(definiend, value);
            }
        } else if (definiend === vm.ignore()) {
            // Ignore.
        } else {
            throw vm.make_type_error(definiend, DEFINIEND_TREE);
        }
        return value;
    };

    /*
     * This error is signalled when a definiend cannot be matched
     * against a value.
     */
    vm.Match_error = class Lisp_match_error extends vm.Error
    {
        constructor(definiend, value)
        {
            super("Match error");
            this.lisp_slot_definiend = definiend;
            this.lisp_slot_value = value;
        }
    };

    /*** Operators ***/

    /*
     * Superclass of everything that computes.
     *
     * An operator receives an operand and should do something with it
     * in some environment.  Usually, the operand will be a list, but
     * this is not strictly required by the semantics.
     *
     * There are three types of operators:
     *
     * 1) Built-in operators that are written in JavaScript.
     *
     * 2) Fexprs that are written in Lisp.
     *
     * 3) Functions that wrap around an underlying operator.
     */
    vm.Operator = class Lisp_operator extends vm.Object
    {
        /*
         * Compute on the operand in the environment and return a result.
         */
        operate(operand, env) { vm.abstract_method(); }

        /*
         * Subclasses should override this with an informative result.
         */
        get_name() { return vm.sym("anonymous operator"); }
    };

    /*
     * A fexpr is an operator that does not evaluate its operand by
     * default.
     */
    vm.Fexpr = class Lisp_fexpr extends vm.Operator
    {
        /*
         * Constructs a new fexpr with the given parameter tree,
         * environment parameter, body form, and definition environment.
         *
         * The parameter tree is used to destructure the operand.
         *
         * The environment parameter is bound to the lexical environment
         * in which the fexpr is called (aka the "dynamic environment").
         *
         * The body form is the expression that gets evaluated.
         *
         * The definition environment remembers the lexical environment
         * in which the fexpr was created (aka the "static environment").
         * The body form gets evaluated in a child environment of the
         * definition environment.
         */
        constructor(param_tree, env_param, body_form, def_env)
        {
            super();
            this.param_tree = vm.assert_type(param_tree, DEFINIEND_TREE);
            this.env_param = vm.assert_type(env_param, DEFINIEND);
            this.body_form = vm.assert_type(body_form, vm.TYPE_ANY);
            this.def_env = vm.assert_type(def_env, vm.Environment);;
        }

        /*
         * The body form of a fexpr gets evaluated in a fresh child
         * environment of the static environment in which the fexpr
         * was defined (this gives the usual lexical scope semantics
         * where the form can access bindings from outer
         * environments).
         *
         * The operand gets matched against the fexpr's parameter
         * tree.  The dynamic environment in which the fexpr is called
         * gets matched against the fexpr's environment parameter.
         * Resulting bindings are placed into the child environment.
         */
        operate(operand, dyn_env)
        {
            const child_env = vm.make_environment(this.def_env);
            vm.match(this.param_tree, operand, child_env);
            vm.match(this.env_param, dyn_env, child_env);
            return vm.eval(this.body_form, child_env);
        }
    };

    /*
     * A function is an operator what wraps around another operator
     * (typically a built-in-operator or fexpr, but another function
     * is also possible), and evaluates the operands before passing
     * them on to the wrapped operator.
     *
     * The evaluated operands of a function are called arguments.
     *
     * Note that a function's operand must be a list, while this
     * is not required for operators in general.
     */
    vm.Function = class Lisp_function extends vm.Operator
    {
        /*
         * Constructs a new function with the given underlying operator.
         */
        constructor(operator)
        {
            super();
            vm.assert_type(operator, vm.Operator);
            this.wrapped_operator = operator;
        }

        /*
         * Evaluate the operands to yield a list of arguments, and
         * then call the underlying, wrapped operator with the
         * arguments.
         */
        operate(operands, env)
        {
            return vm.bind(() => eval_args(operands, vm.nil()),
                           (args) => vm.operate(this.wrapped_operator, args, env));

            function eval_args(todo, done)
            {
                if (todo === vm.nil())
                    return vm.reverse(done);
                else
                    return vm.bind(() => vm.eval(todo.car(), env),
                                   (arg) => eval_args(todo.cdr(), vm.cons(arg, done)));
            }
        }

        /*
         * Returns the wrapped operator underlying the function.
         */
        unwrap()
        {
            return this.wrapped_operator;
        }

        /*
         * Returns the name of the function, which is the name
         * of the underlying operator.
         */
        get_name()
        {
            return this.unwrap().get_name();
        }
    };

    /*
     * Wraps a function around an operator.
     */
    vm.wrap = (operator) => new vm.Function(operator);

    /*
     * Built-in operators are operators that are implemented in JS.
     */
    vm.Built_in_operator = class Lisp_built_in_operator extends vm.Operator
    {
        constructor(operate_function, name = "anonymous built-in operator")
        {
            super();
            vm.assert_type(operate_function, "function");
            vm.assert_type(name, "string");
            this.operate_function = operate_function;
            this.name = vm.sym(name);
        }

        operate(operands, env)
        {
            return this.operate_function(operands, env);
        }

        get_name() { return this.name; }
    };

    /*
     * Creates a new built-in operator with the given underlying
     * JS function and name.
     */
    vm.built_in_operator = (fun, name) => new vm.Built_in_operator(fun, name);

    /*
     * Creates a new built-in function, that is, a wrapped built-in operator.
     */
    vm.built_in_function = (fun, name) => vm.wrap(vm.built_in_operator(fun, name));

    /*
     * Creates a new alien operator, a kind of built-in operator.
     *
     * Despite their name, alien operators are just a slight variation
     * on built-in operators.
     *
     * Their underlying JS function does not, as in the case with
     * normal operators, receive the operand and environment as its
     * two JS arguments, but rather receives the whole list of
     * operands as its JS arguments.  It does not receive the
     * environment as an argument.
     *
     * ---
     *
     * Why aren't alien operators just called JS operators?  Well, we
     * also need alien functions (see below), which would need to be
     * called JS functions for consistency.  But that would obviously
     * be highly confusing.
     */
    vm.alien_operator = (fun, name) =>
    {
        vm.assert_type(fun, "function");
        function operate_function(operands, ignored_env)
        {
            return fun.apply(null, vm.list_to_array(operands));
        }
        return vm.built_in_operator(operate_function, name);
    };

    /*
     * Creates a new alien function, that is, a wrapped alien operator.
     *
     * Alien functions are the main mechanism for exposing JS
     * functions to Lisp.
     */
    vm.alien_function = (fun, name) => vm.wrap(vm.alien_operator(fun, name));

    /*** The Built-In Operators ***/

    /*
     * (%%vau param-tree env-param body-form) => fexpr
     *
     * Built-in operator that creates a new fexpr with the given
     * parameter tree, environment parameter, and body form.
     *
     * The dynamic environment of the call to %%VAU becomes
     * the static environment of the created fexpr.
     */
    vm.VAU = (operands, dyn_env) =>
    {
        const param_tree = vm.elt(operands, 0);
        const env_param = vm.elt(operands, 1);
        const body_form = vm.elt(operands, 2);
        const def_env = dyn_env;
        return new vm.Fexpr(param_tree, env_param, body_form, def_env);
    };

    /*
     * (%%def definiend expression) => result
     *
     * Built-in operator that evaluates the expression and matches the
     * definiend against the result value.  Bindings are placed into
     * the dynamic environment in which %%DEF is called.
     *
     * Returns the value.
     */
    vm.DEF = (operands, env) =>
    {
        const definiend = vm.elt(operands, 0);
        const expression = vm.elt(operands, 1);

        return vm.bind(() => vm.eval(expression, env),
                       (result) => vm.match(definiend, result, env));
    };

    /*
     * (%%progn . forms) => result
     *
     * Built-in operator that evaluates the forms from left to right
     * and returns the result of the last one.
     *
     * Returns #VOID if there are no forms.
     */
    vm.PROGN = (forms, env) =>
    {
        if (forms === vm.nil())
            return vm.void();
        else
            return progn(forms);

        function progn(forms)
        {
            return vm.bind(() => vm.eval(forms.car(), env),
                           (result) => {
                               if (forms.cdr() === vm.nil())
                                   return result;
                               else
                                   return progn(forms.cdr());
                           });
        }
    };

    /*
     * (%%if test consequent alternative) => result
     *
     * First, evaluates the test expression which must yield a
     * boolean.
     *
     * Then, evaluates either the consequent or alternative expression
     * depending on the result of the test expression, and returns its
     * result.
     */
    vm.IF = (operands, env) =>
    {
        const test = vm.elt(operands, 0);
        const consequent = vm.elt(operands, 1);
        const alternative = vm.elt(operands, 2);

        return vm.bind(() => vm.eval(test, env),
                       (result) => {
                           vm.assert_type(result, vm.Boolean);
                           if (result == vm.t())
                               return vm.eval(consequent, env);
                           else
                               return vm.eval(alternative, env);
                       });
    };

    /*** Generic Functions ***/

    /*
     * (%%invoke-method method-name method-args) => result
     *
     * Built-in function used in the implementation of generic
     * functions that invokes a method.
     *
     * The first element of method-args must be the receiver object.
     */
    vm.INVOKE_METHOD = (args, env) =>
    {
        const method_name = vm.assert_type(vm.elt(args, 0), vm.Symbol);
        const method_args = vm.assert_type(vm.elt(args, 1), vm.Cons);
        const receiver = method_args.car();
        const receiver_class = vm.class_of(receiver);
        const method = receiver_class.lookup_method(receiver, method_name);
        /*
         * The dynamic environment isn't really needed here, this could also
         * use a fresh, empty environment.
         */
        return vm.operate(method, method_args, env);
    };

    /*** Exception Trapping and Panicking ***/

    /*
     * All exceptions - except nonlocal exits (see control.mjs) and
     * panics (see below) - that happen during evaluation are caught
     * by this function.
     *
     * If the user defined error handler, ERROR, is defined in the
     * VM's root environment, it is called with the exception as
     * argument.  This unifies the JS exception system and the Lisp
     * condition system.
     *
     * If the user defined error handler is not defined (as is the
     * case during boot), the exception gets wrapped in a panic and
     * rethrown.
     */
    vm.trap_exceptions = (thunk) =>
    {
        try {
            return thunk();
        } catch (e) {
            if ((e instanceof vm.Nonlocal_exit) || (e instanceof vm.Panic))
                throw e;
            else
                return vm.call_user_error_handler(e);
        }
    };

    /*
     * Calls the user error handler function, ERROR, defined in Lisp
     * with an exception, or panics with the exception if it is not
     * defined.
     */
    vm.call_user_error_handler = (exception) =>
    {
        const env = vm.get_environment();
        const sym = vm.fsym("error");
        if (env.is_bound(sym)) {
            return vm.operate(env.lookup(sym), vm.list(exception), vm.make_environment());
        } else {
            vm.panic(exception);
        }
    };

    /*
     * A panic is an error that is not caught by the usual exception
     * trapping mechanism.
     */
    vm.Panic = class Panic extends Error
    {
        constructor(cause)
        {
            if (cause && cause.message)
                super("LISP panic: " + cause.message);
            else
                super("LISP panic!");
            this.cause = cause;
        }
    };

    /*
     * Throws a new panic with the given exception as cause.
     */
    vm.panic = (exception) =>
    {
        throw new vm.Panic(exception);
    };

    /*** Definition Utilities ***/

    /*
     * Registers a global variable in the VM's root environment.
     */
    vm.define_variable = (name, object) =>
    {
        vm.get_environment().put(vm.sym(name), object);
    };

    /*
     * Registers a constant in the VM's root environment.
     */
    vm.define_constant = (name, object) =>
    {
        vm.define_variable(name, object);
    };

    /*
     * Registers an operator in the VM's root environment.
     */
    vm.define_operator = (operator) =>
    {
        vm.get_environment().put(operator.get_name().to_function_symbol(),
                                 operator);
    };

    /*
     * Shorthand for registering a built-in operator in the VM's root environment.
     */
    vm.define_built_in_operator = (name, fun) =>
    {
        vm.define_operator(vm.built_in_operator(fun, name));
    };

    /*
     * Shorthand for registering a built-in function in the VM's root environment.
     */
    vm.define_built_in_function = (name, js_fun) =>
    {
        vm.define_operator(vm.built_in_function(js_fun, name));
    };

    /*
     * Shorthand for registering an alien function in the VM's root environment.
     */
    vm.define_alien_function = (name, js_fun) =>
    {
        vm.define_operator(vm.alien_function(js_fun, name));
    };

    /*** Lisp API ***/

    vm.define_class("operator", vm.Operator);

    vm.define_class("built-in-operator", vm.Built_in_operator, vm.Operator);

    vm.define_class("fexpr", vm.Fexpr, vm.Operator);

    vm.define_class("function", vm.Function, vm.Operator);

    vm.define_condition("match-error", vm.Match_error, vm.Error);

    vm.define_built_in_operator("%%vau", vm.VAU);

    vm.define_built_in_operator("%%def", vm.DEF);

    vm.define_built_in_operator("%%progn", vm.PROGN);

    vm.define_built_in_operator("%%if", vm.IF);

    vm.define_alien_function("%%wrap", (operator) => vm.wrap(operator));

    vm.define_alien_function("%%unwrap", (fun) => vm.assert_type(fun, vm.Function).unwrap());

    vm.define_alien_function("%%eval", (expr, env) => vm.eval(expr, env));

    vm.define_alien_function("%%eq", (a, b) => vm.to_lisp_boolean(a === b));

    vm.define_alien_function("%%=", (a, b) => vm.to_lisp_boolean(vm.equal(a, b)));

    vm.define_alien_function("%%<", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) < 0));

    vm.define_alien_function("%%>", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) > 0));

    vm.define_alien_function("%%<=", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) <= 0));

    vm.define_alien_function("%%>=", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) >= 0));

    vm.define_alien_function("%%+", (a, b) => vm.add(a, b));

    vm.define_alien_function("%%-", (a, b) => vm.subtract(a, b));

    vm.define_alien_function("%%*", (a, b) => vm.multiply(a, b));

    vm.define_alien_function("%%/", (a, b) => vm.divide(a, b));

    vm.define_alien_function("%%cons", (car, cdr) => vm.cons(car, cdr));

    vm.define_alien_function("%%car", (cons) => vm.assert_type(cons, vm.Cons).car());

    vm.define_alien_function("%%cdr", (cons) => vm.assert_type(cons, vm.Cons).cdr());

    vm.define_alien_function("%%intern", (string) => vm.intern(string));

    vm.define_alien_function("%%symbol-name", (sym) =>
        vm.assert_type(sym, vm.Symbol).get_string());

    vm.define_alien_function("%%variable-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_variable_symbol());

    vm.define_alien_function("%%function-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_function_symbol());

    vm.define_alien_function("%%class-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_class_symbol());

    vm.define_alien_function("%%keyword-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_keyword_symbol());

    vm.define_alien_function("%%make-environment", (parent = null) =>
        vm.make_environment(parent));

    vm.define_alien_function("%%boundp", (sym, env) =>
        vm.to_lisp_boolean(vm.assert_type(env, vm.Environment).is_bound(sym)));

    vm.define_alien_function("%%class-of", (obj) => vm.class_of(obj));

    vm.define_alien_function("%%typep", (obj, cls) =>
        vm.to_lisp_boolean(vm.is_subclass(vm.class_of(obj), cls)));

    vm.define_alien_function("%%make-instance", (cls, ...slot_inits) =>
        vm.make_instance(cls, ...slot_inits));

    vm.define_alien_function("%%slot-value", (obj, slot_name) =>
        vm.assert_type(obj, vm.Standard_object).slot_value(slot_name));

    vm.define_alien_function("%%set-slot-value", (obj, slot_name, slot_value) =>
        vm.assert_type(obj, vm.Standard_object).set_slot_value(slot_name, slot_value));

    vm.define_alien_function("%%slot-bound-p", (obj, slot_name) =>
        vm.to_lisp_boolean(vm.assert_type(obj, vm.Standard_object).is_slot_bound(slot_name)));

    vm.define_alien_function("%%put-method", (cls, name, method) =>
        vm.assert_type(cls, vm.Class).put_method(name, method));

    vm.define_built_in_function("%%invoke-method", vm.INVOKE_METHOD);

    vm.define_alien_function("%%make-standard-class", (name, super_class) =>
        vm.make_standard_class(name, super_class));

    vm.define_alien_function("%%class-name", (cls) =>
        vm.assert_type(cls, vm.Class).get_name());

    vm.define_alien_function("%%subclassp", (sub_cls, super_cls) =>
        vm.to_lisp_boolean(vm.is_subclass(sub_cls, super_cls)));

    vm.define_alien_function("%%panic", (exception) => vm.panic(exception));

};


/***/ }),

/***/ "./src/js.mjs":
/*!********************!*\
  !*** ./src/js.mjs ***!
  \********************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_js": () => (/* binding */ init_js)
/* harmony export */ });
/*
 * LispX JavaScript Interface
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds JS interface functionality to a virtual machine.
 */
function init_js(vm)
{
    /*
     * Transforms a JS boolean into a Lisp one.
     */
    vm.to_lisp_boolean = (js_bool) =>
    {
        return vm.assert_type(js_bool, "boolean") ? vm.t() : vm.f();
    };

    /*
     * Invokes a JS method on an object from Lisp.
     */
    vm.apply_js_method = (receiver, method_name, args) =>
    {
        vm.assert_type(method_name, vm.String);
        vm.assert_type(args, vm.List);
        /*
         * Could throw a more helpful error here if the method isn't found.
         */
        const method = vm.assert_type(receiver[method_name.to_js_string()], "function");
        return method.apply(receiver, vm.list_to_array(args))
    };

    /*
     * Accesses a global variable by name.
     */
    vm.js_global = (name) =>
    {
        vm.assert_type(name, vm.String);
        return globalThis[name.to_js_string()];
    };

    /*
     * Calls a JS constructor with arguments.
     *
     * Note that this is not a fat arrow function because we need
     * access to the arguments object (although it's probably possible
     * to use ... syntax and make this a fat arrow function).
     */
    vm.js_new = function(constructor /* , arg1, ..., argN */)
    {
        vm.assert_type(constructor, "function");
        /*
         * See https://stackoverflow.com/a/23190790
         */
        const factory_function = constructor.bind.apply(constructor, arguments);
        return new factory_function();
    };

    /*
     * Returns a named property of an object.
     */
    vm.js_get = (object, prop_name) =>
    {
        vm.assert_type(prop_name, vm.String);
        return object[prop_name.to_js_string()];
    };

    /*
     * Returns an indexed element of a JS array.
     */
    vm.js_elt = (js_array, index) =>
    {
        vm.assert(Array.isArray(js_array));
        vm.assert_type(index, vm.Number);
        return js_array[index.to_js_number()];
    };

    /*
     * Makes a JS function callable as a Lisp one.
     */
    vm.to_lisp_function = (js_fun) =>
    {
        return vm.alien_function(js_fun, "anonymous JS function");
    };

    /*
     * Makes a Lisp operator callable as a JS function.
     */
    vm.to_js_function = (operator) =>
    {
        vm.assert_type(operator, vm.Operator);
        return function()
        {
            var args = vm.array_to_list(Array.prototype.slice.call(arguments));
            return vm.operate(operator, args, vm.make_environment());
        };
    };

    /*** Lisp API ***/

    vm.define_constant("+js-true+", true);

    vm.define_constant("+js-false+", false);

    vm.define_constant("+js-null+", null);

    vm.define_constant("+js-undefined+", undefined);

    vm.define_alien_function("%%to-lisp-boolean", vm.to_lisp_boolean);

    vm.define_alien_function("%%to-js-boolean", (bool) =>
        vm.assert_type(bool, vm.Boolean).to_js_boolean());

    vm.define_alien_function("%%to-lisp-number", (js_num) =>
        vm.num(vm.assert_type(js_num, "number")));

    vm.define_alien_function("%%to-js-number", (num) =>
        vm.assert_type(num, vm.Number).to_js_number());

    vm.define_alien_function("%%to-lisp-string", (js_str) =>
        vm.str(vm.assert_type(js_str, "string")));

    vm.define_alien_function("%%to-js-string", (str) =>
        vm.assert_type(str, vm.String).to_js_string());

    vm.define_alien_function("%%to-lisp-function", vm.to_lisp_function);

    vm.define_alien_function("%%to-js-function", vm.to_js_function);

    vm.define_alien_function("%%js-global", vm.js_global);

    vm.define_alien_function("%%js-new", vm.js_new);

    vm.define_alien_function("%%js-get", vm.js_get);

    vm.define_alien_function("%%js-elt", vm.js_elt);

    vm.define_alien_function("%%list-to-js-array", vm.list_to_array);

    vm.define_alien_function("%%js-array-to-list", vm.array_to_list);

    vm.define_alien_function("%%apply-js-method", vm.apply_js_method);

    vm.define_alien_function("%%js-log", (...objects) => console.log(...objects));

    vm.define_alien_function("%%sleep", (ms) => {
        vm.assert_type(ms, vm.Number);
        return new Promise(resolve => setTimeout(resolve, ms.to_js_number()));
    });

};


/***/ }),

/***/ "./src/print.mjs":
/*!***********************!*\
  !*** ./src/print.mjs ***!
  \***********************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_print": () => (/* binding */ init_print)
/* harmony export */ });
/*
 * LispX Printer
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds printing functionality to a virtual machine.
 */
function init_print(vm)
{
    /*** Printer Algorithm ***/

    /*
     * If true, objects are printed so that they can be read back in
     * by READ. If false, objects are printed so that they look good
     * on the teletype.
     */
    vm.PRINT_ESCAPE = vm.make_dynamic(vm.t());

    /*
     * How many levels should be printed before abbreviating with "#"?
     *
     * If false, do not abbreviate.
     */
    vm.PRINT_LEVEL = vm.make_dynamic(vm.f());

    /*
     * The current level we are printing at, to be compared against
     * *PRINT-LEVEL*.  Increased every time WRITE is entered, so
     * effectively starts at 0 for the outermost object.
     */
    vm.CURRENT_PRINT_LEVEL = vm.make_dynamic(vm.num(-1));

    /*
     * Write object to stream.  Main printer entry point.
     */
    vm.write = (object, stream) =>
    {
        vm.assert_type(object, vm.TYPE_ANY);
        vm.assert_type(stream, vm.Output_stream);
        call_with_increased_current_print_level(() => {
            if (vm.is_lisp_object(object))
                object.write_object(stream);
            else
                vm.write_js_object(object, stream);
        });
        return object;

        /*
         * On every call to WRITE, increase the current print level.
         */
        function call_with_increased_current_print_level(thunk)
        {
            const new_level = vm.add(vm.CURRENT_PRINT_LEVEL.get_value(), vm.num(1));
            vm.progv([vm.CURRENT_PRINT_LEVEL], [new_level], thunk);
        }
    };

    /*
     * All non-Lisp objects print as "#<object>" for now.
     */
    vm.write_js_object = (object, stream) =>
    {
        vm.write_unreadable_object(object, stream);
    };

    /*
     * Write an unreadable object.  If thunk is non-null, it can
     * print extra stuff, cf. CL's PRINT-UNREADABLE-OBJECT.
     */
    vm.write_unreadable_object = (object, stream, thunk = null) =>
    {
        vm.assert_type(object, vm.TYPE_ANY);
        vm.assert_type(stream, vm.Output_stream);
        stream.write_string(vm.str("#<"));
        stream.write_string(vm.class_of(object).get_name().get_string());
        if (thunk !== null) {
            thunk();
        }
        stream.write_byte(">");
    };

    /*** Built-In Object Writing Functions ***/

    vm.Object.prototype.write_object = function(stream)
    {
        vm.write_unreadable_object(this, stream);
    };

    vm.Boolean.prototype.write_object = function(stream)
    {
        stream.write_string((this === vm.t()) ? vm.str("#t") : vm.str("#f"));
    };

    vm.Nil.prototype.write_object = function(stream)
    {
        stream.write_string(vm.str("#nil"));
    };

    vm.Void.prototype.write_object = function(stream)
    {
        stream.write_string(vm.str("#void"));
    };

    vm.Ignore.prototype.write_object = function(stream)
    {
        stream.write_string(vm.str("#ignore"));
    };

    vm.Number.prototype.write_object = function(stream)
    {
        stream.write_string(this.to_string());
    };

    vm.Symbol.prototype.write_object = function(stream)
    {
        if (vm.PRINT_ESCAPE.get_value() === vm.t()) {
            stream.write_string(get_symbol_prefix(this));
            if (symbol_needs_escaping(this)) {
                write_delimited(this.get_string(), stream, "|");
            } else {
                stream.write_string(this.get_string());
            }
        } else {
            stream.write_string(this.get_string());
        }
    };

    vm.String.prototype.write_object = function(stream)
    {
        if (vm.PRINT_ESCAPE.get_value() === vm.t()) {
            write_delimited(this, stream, "\"");
        } else {
            stream.write_string(this);
        }
    };

    /*
     * Writes the contents of strings and escaped symbols.
     */
    function write_delimited(string, stream, delimiter)
    {
        stream.write_byte(delimiter);
        const bytes = string.get_utf8_bytes();
        for (let i = 0; i < bytes.length; i++) {
            const b = bytes[i];
            if ((b === delimiter) || (b === "\\")) {
                stream.write_byte("\\");
                stream.write_byte(b);
            } else {
                stream.write_byte(b);
            }
        }
        stream.write_byte(delimiter);
    }

    /*
     * Returns the prefix identifying the namespace of the symbol.
     */
    function get_symbol_prefix(sym)
    {
        switch (sym.get_namespace()) {
        case vm.VARIABLE_NAMESPACE: return vm.str("");
        case vm.FUNCTION_NAMESPACE: return vm.str("#'");
        case vm.CLASS_NAMESPACE: return vm.str("#$");
        case vm.KEYWORD_NAMESPACE: return vm.str(":");
        default: vm.panic("Unknown symbol namespace");
        }
    }

    /*
     * A symbol needs escaping if it could be parsed as a number, or
     * if it contains whitespace or terminating characters, or the
     * escape character '\'.
     */
    function symbol_needs_escaping(sym)
    {
        const bytes = sym.get_string().get_utf8_bytes();
        if (vm.parses_as_number(bytes))
            return true;
        else
            for (let i = 0; i < bytes.length; i++) {
                const b = bytes[i];
                if (vm.is_whitespace(b)
                    || vm.is_terminating_character(b)
                    || b === "\\")
                    return true;
            }
        return false;
    }

    vm.Cons.prototype.write_object = function(stream)
    {
        function write_cons(cons, stream)
        {
            if (cons.cdr() === vm.nil()) {
                vm.write(cons.car(), stream);
            } else if (cons.cdr() instanceof vm.Cons) {
                vm.write(cons.car(), stream);
                stream.write_byte(" ");
                write_cons(cons.cdr(), stream);
            } else {
                vm.write(cons.car(), stream);
                stream.write_string(vm.str(" . "));
                vm.write(cons.cdr(), stream);
            }
        }
        maybe_abbreviate_object_based_on_current_print_level(stream, () => {
            stream.write_byte("(");
            write_cons(this, stream);
            stream.write_byte(")");
        });
    };

    vm.Standard_object.prototype.write_object = function(stream)
    {
        const prefix = "lisp_slot_";
        maybe_abbreviate_object_based_on_current_print_level(stream, () => {
            vm.write_unreadable_object(this, stream, () => {
                for (const name of Object.getOwnPropertyNames(this).sort()) {
                    if (name.startsWith(prefix)) {
                        stream.write_byte(" ");
                        vm.write(vm.kwd(name.slice(prefix.length)), stream);
                        stream.write_byte(" ");
                        vm.write(this[name], stream);
                    }
                }
            });
        });
    };

    /*
     * Utility for abbreviating objects based on their nesting level.
     *
     * Call the thunk if *PRINT-LEVEL* is false, or if
     * *CURRENT-PRINT-LEVEL* is less than *PRINT-LEVEL*.
     *
     * Otherwise, print "#" to the stream.
     */
    function maybe_abbreviate_object_based_on_current_print_level(stream, thunk)
    {
        const print_level = vm.PRINT_LEVEL.get_value();
        const current_print_level = vm.CURRENT_PRINT_LEVEL.get_value();
        if ((print_level === vm.f())
            || (vm.compare(current_print_level, print_level) < 0)) {
            thunk();
        } else {
            stream.write_byte("#");
        }
    }

    /*** Utilities ***/

    vm.write_to_string = (object) =>
    {
        const st = new vm.String_output_stream();
        vm.write(object, st);
        return st.get_string();
    };

    vm.write_to_js_string = (object) =>
    {
        return vm.write_to_string(object).to_js_string();
    };

    /*** Lisp API ***/

    vm.define_variable("*print-escape*", vm.PRINT_ESCAPE);

    vm.define_variable("*print-level*", vm.PRINT_LEVEL);

    vm.define_alien_function("%%write", (object, stream) => vm.write(object, stream));

};


/***/ }),

/***/ "./src/read.mjs":
/*!**********************!*\
  !*** ./src/read.mjs ***!
  \**********************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_read": () => (/* binding */ init_read)
/* harmony export */ });
/*
 * LispX Reader
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds reading functionality to a virtual machine.
 *
 * ----
 *
 * A note on bytes versus characters: Unlike Common Lisp, LispX
 * currently has no notion of characters, it knows only (UTF-8) bytes.
 * Streams are byte-based, too.
 *
 * The reader code exploits the fact that ASCII characters encode to
 * the same code units in UTF-16 and UTF-8, and uses literals like "("
 * directly.
 *
 * This whole setup works passably because all syntactically
 * meaningful (aka macro) characters like (, ), ", etc are from the
 * ASCII range.  It would not work well if we wanted to be able to do
 * things like use non-ASCII characters as macro characters.
 *
 * The code sometimes uses the term character for a byte, but only
 * for ASCII bytes, so it's not really wrong.
 */
function init_read(vm)
{
    /*** Reader Algorithm ***/

    /*
     * Cache symbols that get used repeatedly.
     */
    const DOT = vm.sym(".");
    const QUOTE = vm.sym("quote");

    /*
     * Main reader entry point and recursively used read function.
     *
     * Attempts to read one object from the stream.
     *
     * If EOF is reached before an object has been read, and
     * eof_error_p is true, throws an error.  If false, returns
     * eof_value.  eof_error_p defaults to true.  eof_value defaults
     * to #VOID.
     *
     * It throws an error, regardless of eof_error_p, if the stream
     * ends in the middle of an object representation.  For example,
     * if a stream does not contain enough right parentheses to
     * balance the left parentheses in it, read throws an error.
     *
     * The consing dot as a token is invalid at this main entry point,
     * because it makes sense only inside lists, of course.
     */
    vm.read = (stream, eof_error_p = true, eof_value = vm.void()) =>
    {
        vm.assert_type(stream, vm.Input_stream);
        vm.assert_type(eof_error_p, "boolean");
        vm.assert_type(eof_value, vm.TYPE_ANY);

        const unique_eof = {};
        const obj = read_allowing_dot(stream, false, unique_eof);
        if (obj === unique_eof)
            return vm.eof(eof_error_p, eof_value);
        else if (obj === DOT)
            throw new vm.Reader_error("Consing dot not allowed here");
        else
            return obj;
    };

    /*
     * Main body of the reader code without prohibition against the
     * consing dot as a token.
     *
     * This is used solely inside read_list() directly, where the
     * consing dot is allowed; all other places use read(), which
     * wraps around this and disallows the dot.
     */
    function read_allowing_dot(stream, eof_error_p = true, eof_value = vm.void())
    {
        const b = vm.skip_whitespace(stream, false);
        if (b === vm.void()) {
            return vm.eof(eof_error_p, eof_value);
        } else {
            switch (b) {
            case "(":
                return vm.read_delimited_list(stream, ")");
            case "\"":
                return read_string(stream);
            case "'":
                return read_quote(stream);
            case ":":
                return read_namespaced_symbol(stream, vm.KEYWORD_NAMESPACE);
            case "#":
                return read_sharpsign(stream);
            case "|":
                return read_escaped_symbol(stream);
            case ")":
                throw new vm.Reader_error("Unbalanced parenthesis");
            default:
                if (vm.is_macro_character(b)) {
                    return vm.call_reader_macro(stream, b);
                } else {
                    stream.unread_byte(b);
                    return read_token(stream);
                }
            }
        }
    }

    /*
     * Signalled when a syntax error occurs.
     */
    vm.Reader_error = class Lisp_reader_error extends vm.Error
    {
        constructor(message)
        {
            super(message);
        }
    };

    /*** Reading Tokens ***/

    /*
     * Reads a token, that is, either a number or an unescaped symbol.
     *
     * There must be at least one byte in the stream, which is ensured
     * by the caller, read_allowing_dot().
     */
    function read_token(stream)
    {
        let bytes = "";
        while (true) {
            const b = stream.read_byte(false);
            if ((b === vm.void()) || vm.is_whitespace(b)) {
                // The token is terminated by EOF or whitespace.
                break;
            } else if (vm.is_terminating_character(b)) {
                // The token is terminated by a terminating character.
                // Unread it.
                stream.unread_byte(b);
                break;
            } else if (b === "\\") {
                // Escape character.  Throws an EOF error if there is
                // no escaped character following.
                bytes += read_escape_character(stream);
            } else {
                // Normal character.
                bytes += b;
            }
        }
        if (vm.parses_as_number(bytes))
            return vm.num(bytes);
        else
            return vm.intern(new vm.String(bytes));
    }

    /*
     * Reads an escape char after \.
     *
     * Throws an error if EOF is reached.
     */
    function read_escape_character(stream)
    {
        const b = stream.read_byte(true);
        switch (b) {
        case 'n':  return '\n';
        case 't':  return '\t';
        case '"':  return '"';
        case '\\': return '\\';
        case '|':  return '|';
        default:   throw new vm.Reader_error(`Invalid escape character ${b}`);
        }
    }

    /*
     * Returns true if a byte terminates a token, false otherwise.
     */
    vm.is_terminating_character = (b) =>
    {
        switch(b) {
        case "(":
        case ")":
        case ";":
        case "\"":
        case "\'":
        case "|":
            return true;
        default:
            return vm.is_terminating_macro_character(b);
        }
    };

    /*
     * Returns true if the bytes can be parsed as a number, false
     * otherwise.
     */
    vm.parses_as_number = (bytes) =>
    {
        return /^-?\d+(\.\d+)?$/.test(bytes);
    };

    /*** Reading Lists ***/

    /*
     * Reads a list terminated by the end character, typically ")".
     *
     * Throws an EOF error if the list is incomplete.
     */
    vm.read_delimited_list = (stream, end_char) =>
    {
        // We keep track of the list start and end with two mutable
        // references.
        let start = vm.nil();
        let end = start;
        while (true) {
            // Read the next byte.
            const b = vm.skip_whitespace(stream, true);
            if (b === end_char) {
                // It's a closing parenthesis, the list ends.
                return start;
            } else {
                // Otherwise, read the list element.
                stream.unread_byte(b);
                const obj = read_allowing_dot(stream, true);
                if (obj === DOT) {
                    // We have a consing dot.
                    if (start === vm.nil()) {
                        // Can't be the first element of the list.
                        throw new vm.Reader_error("Consing dot at start of list");
                    } else {
                        // The consing dot must be followed by a
                        // single object, which becomes the cdr of the
                        // list.
                        end.set_cdr(vm.read(stream, true));
                        // The list must end now or it's an error.
                        const c = vm.skip_whitespace(stream, true);
                        if (c === end_char)
                            return start;
                        else
                            throw new vm.Reader_error("Multiple objects after consing dot");
                    }
                } else {
                    // We have a normal list element, cons it up.
                    const cons = vm.cons(obj, vm.nil());
                    if (start === vm.nil()) {
                        // It's the first element, remember it in start.
                        start = cons;
                        end = start;
                    } else {
                        // It's a later element, add it at the end.
                        end.set_cdr(cons);
                        end = cons;
                    }
                }
            }
        }
    }

    /*** Reading Strings and Escaped Symbols ***/

    /*
     * Reads a string.
     */
    function read_string(stream)
    {
        return read_delimited(stream, "\"");
    }

    /*
     * Reads an escaped symbol.
     */
    function read_escaped_symbol(stream)
    {
        return vm.intern(read_delimited(stream, "|"));
    }

    /*
     * Reading code for strings and escaped symbols.
     *
     * Returns a string.
     *
     * Throws an error if EOF is reached.
     */
    function read_delimited(stream, end_char)
    {
        let bytes = "";
        while (true) {
            const b = stream.read_byte(true);
            if (b === end_char) {
                break;
            } else if (b === "\\") {
                bytes += read_escape_character(stream);
                continue;
            } else {
                bytes += b;
                continue;
            }
        }
        return new vm.String(bytes);
    }

    /*** Reading Quote ***/

    /*
     * 'FOO reads as (quote FOO).
     */
    function read_quote(stream)
    {
        return vm.list(QUOTE, vm.read(stream, true));
    }

    /*** Reading Sharpsign ***/

    /*
     * The sharpsign (#) introduces various special syntaxes.
     */
    function read_sharpsign(stream)
    {
        const b = stream.read_byte(true);
        if (is_alpha(b)) {
            stream.unread_byte(b);
            return read_constant(stream);
        } else {
            switch (b) {
            case "'":
                return read_namespaced_symbol(stream, vm.FUNCTION_NAMESPACE);
            default:
                throw new vm.Reader_error(`Illegal dispatching character ${b}`);
            }
        }
    }

    /*
     * Test whether a byte can be considered alphabetic for the
     * purposes of parsing sharpsign constants like #T.
     */
    function is_alpha(b)
    {
        return /^[a-z]$/.test(b);
    }

    /*
     * Reads a constant like #T.
     */
    function read_constant(stream)
    {
        const sym = vm.read(stream, true);
        vm.assert_type(sym, vm.Symbol);
        const constant = CONSTANTS[sym.get_string().get_utf8_bytes()];
        if (constant)
            return constant;
        else
            throw new vm.Reader_error("Illegal constant");
    }

    /*
     * If a symbol immediately follows # it must be one of those.
     */
    const CONSTANTS = {
        "t": vm.t(),
        "f": vm.f(),
        "nil": vm.nil(),
        "void": vm.void(),
        "ignore": vm.ignore(),
    };

    /*
     * Reads namespaced symbols, such as #'FUNCTION and :KEYWORD symbols.
     */
    function read_namespaced_symbol(stream, namespace)
    {
        const sym = vm.read(stream, true);
        vm.assert_type(sym, vm.Symbol);
        return sym.to_namespace(namespace);
    }

    /*** Skipping Whitespace and Comments ***/

    /*
     * Skips over whitespace and comments and returns the first
     * non-whitespace byte afterwards.
     */
    vm.skip_whitespace = (stream, eof_error_p = true, eof_value = vm.void()) =>
    {
        while (true) {
            const b = stream.read_byte(false);
            if (b === vm.void()) {
                // We have reached EOF, do the usual handling.
                return vm.eof(eof_error_p, eof_value);
            } else if (vm.is_whitespace(b)) {
                // It's whitespace, go to next byte.
                continue;
            } else if (b === ";") {
                // It starts a line comment.
                line_comment: while (true) {
                    const c = stream.read_byte(false);
                    switch (c) {
                    case vm.void():
                        // We have reached EOF, do the usual handling.
                        return vm.eof(eof_error_p, eof_value);
                    case "\n":
                        // It's a newline, the line comment is over.
                        break line_comment;
                    default:
                        // It's any other byte, the line comment continues.
                        continue line_comment;
                    }
                }
            } else if (b === "#") {
                // It might start a block comment.
                const c = stream.read_byte(false);
                if (c === "|") {
                    // Bingo.  Skip the comment and continue after it.
                    skip_block_comment(stream);
                    continue;
                } else if (c === vm.void()) {
                    // Nope, the EOF comes after the #,
                    // so it's the first non-whitespace byte.
                    return b;
                } else {
                    // Nope, some other byte comes after the #,
                    // so it's the first non-whitespace byte.
                    // Unread the other byte.
                    stream.unread_byte(c);
                    return b;
                }
            } else {
                // We have found the first non-whitespace byte.
                return b;
            }
        }
    }

    /*
     * Skips #| block comments #| which may nest |# |#.
     *
     * The initial #| has already been read when this is called.
     */
    function skip_block_comment(stream)
    {
        while (true) {
            const b = stream.read_byte(true);
            if (b === "|") {
                // Character might introduce ending |#
                if (stream.read_byte(true) === "#")
                    break;
                else
                    continue;
            } else if (b === "#") {
                // Character might introduce nested comment #|
                if (stream.read_byte(true) === "|")
                    skip_block_comment(stream);
                continue;
            } else {
                // Anything else just skip.
                continue;
            }
        }
    }

    /*
     * Returns true if a byte is whitespace, false otherwise.
     */
    vm.is_whitespace = (b) =>
    {
        switch(b) {
        case " ":
        case "\n":
        case "\t":
            return true;
        default:
            return false;
        }
    };

    /*** Macro Characters ***/

    /*
     * This is a very bare-bones facility for extending the syntax.
     */

    const READTABLE = {};

    /*
     * Set function as reader macro for the character.
     *
     * It will be called with the input stream and macro character,
     * and should return an option containing the read object.  (In
     * the future it will also be possible to return nil for things
     * like comments, but currently the option must not be nil.)
     */
    vm.set_macro_character = (b, fun, non_terminating_p = false) =>
    {
        READTABLE[b] = { fun: fun, non_terminating_p: non_terminating_p };
    };

    /*
     * Remove the reader macro for the character.
     */
    vm.unset_macro_character = (b) =>
    {
        delete READTABLE[b];
    };

    /*
     * Return true if the character is a macro character, false otherwise.
     */
    vm.is_macro_character = (b) =>
    {
        return READTABLE[b] !== undefined;
    };

    /*
     * Return true if the character is a terminating macro character,
     * false otherwise.
     */
    vm.is_terminating_macro_character = (b) =>
    {
        return vm.is_macro_character(b) && !READTABLE[b].non_terminating_p;
    };

    /*
     * Call the reader macro for the character.
     */
    vm.call_reader_macro = (stream, b) =>
    {
        // Extract content of option
        return READTABLE[b].fun(stream, b).car();
    };

    /*** Read+Eval Utilities ***/

    /*
     * Reads forms from a stream until EOF and evaluates them
     * in a given environment.
     *
     * The environment defaults to the VM's root environment.
     *
     * Returns the value of the last expression or #VOID if the stream
     * is empty.
     */
    vm.eval_stream = function(stream, env = vm.get_environment())
    {
        vm.assert_type(stream, vm.Input_stream);
        vm.assert_type(env, vm.Environment);
        const unique_eof = {};
        let result = vm.void();
        while (true) {
            const form = vm.read(stream, false, unique_eof);
            if (form === unique_eof) {
                break;
            } else {
                /*
                 * Note that we are using vm.eval_form() instead of
                 * vm.eval() here so that suspensions cause an error.
                 */
                result = vm.eval_form(form, env);
            }
        }
        return result;
    }

    /*
     * Evaluates all forms in a string in a given environment.
     *
     * The environment defaults to the VM's root environment.
     */
    vm.eval_string = function(string, env = vm.get_environment())
    {
        return vm.eval_stream(new vm.String_input_stream(string), env);
    }

    /*
     * Evaluates all forms in a JS string in a given environment.
     *
     * The environment defaults to the VM's root environment.
     */
    vm.eval_js_string = function(js_string, env = vm.get_environment())
    {
        return vm.eval_string(vm.str(js_string), env);
    }

    /*** Lisp API ***/

    vm.define_condition("reader-error", vm.Reader_error, vm.Error);

    vm.define_alien_function("%%read", (stream, eof_error_p, eof_value) => {
        vm.assert_type(eof_error_p, vm.Boolean);
        return vm.read(stream, eof_error_p.to_js_boolean(), eof_value);
    });

};

/*
 * Notes
 * -----
 *
 * Using undefined as eof_value will cause #VOID to be
 * used.  I am ignoring this.
 */


/***/ }),

/***/ "./src/seq.mjs":
/*!*********************!*\
  !*** ./src/seq.mjs ***!
  \*********************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_seq": () => (/* binding */ init_seq)
/* harmony export */ });
/*
 * LispX Sequence and List Processing Utilities
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds sequence and list processing utilities to a VM.
 *
 * These are not needed for the internal functioning of the VM, and
 * could be implemented in Lisp, so they reside in their own module.
 */
function init_seq(vm)
{
    /*
     * Creates a list from its arguments so that the last argument
     * becomes the list's final cdr.  Cf. Common Lisp's LIST*.
     */
    vm.list_star = (...objects) =>
    {
        const len = objects.length;
        let list = (len >= 1) ? objects[len - 1] : vm.nil();
        for (let i = len - 1; i > 0; i--)
            list = vm.cons(objects[i - 1], list);
        return list;
    };

    /*
     * Concatenate two lists.  The first one must be proper and is
     * copied.  The second one is not copied (and doesn't even have to
     * be a list). It becomes the cdr of the final cons of the first
     * list, or is returned directly if the first list is empty.
     */
    vm.append = (list1, list2) =>
    {
        vm.assert_type(list1, vm.List);
        if (list1 === vm.nil())
            return list2;
        else
            return vm.cons(list1.car(), vm.append(list1.cdr(), list2));
    };

    /*
     * Returns the length of a list.
     */
    vm.list_length = (list) =>
    {
        vm.assert_type(list, vm.List);
        if (list === vm.nil())
            return 0;
        else
            return 1 + vm.list_length(list.cdr());
    };

    /*
     * Returns the tail of list that would be obtained by calling cdr
     * n times in succession.
     */
    vm.nthcdr = (n, list) =>
    {
        vm.assert_type(list, vm.List);
        if (n === 0) {
            return list;
        } else {
            if (list === vm.nil())
                throw new vm.Out_of_bounds_error();
            else
                return vm.nthcdr(n - 1, list.cdr());
        }
    };

    /*
     * Calls a function on every list element for effect.
     */
    vm.mapc = (fun, list) =>
    {
        if (list !== vm.nil()) {
            fun(list.car());
            vm.mapc(fun, list.cdr());
        }
        return list;
    };

    /*** Common Lisp's SUBSEQ ***/

    /*
     * SUBSEQ for lists.
     */
    vm.list_subseq = (list, start, end = undefined) =>
    {
        vm.assert_type(list, vm.List);
        vm.assert_type(start, "number");

        const tail = vm.nthcdr(start, list);
        if (end === undefined)
            return tail;
        else
            return take_n(tail, end - start);

        function take_n(list, n)
        {
            if (n === 0) {
                return vm.nil();
            } else {
                if (list === vm.nil())
                    throw new vm.Out_of_bounds_error();
                else
                    return vm.cons(list.car(), take_n(list.cdr(), n - 1));
            }
        }
    };

    /*
     * SUBSEQ for strings.
     */
    vm.string_subseq = (string, start, end = undefined) =>
    {
        vm.assert_type(string, vm.String);
        const utf8_bytes = string.get_utf8_bytes();
        return new vm.String(vm.slice_subseq(utf8_bytes, start, end));
    };

    /*
     * Implements SUBSEQ for objects that have a .length property and
     * a slice() method, like JS strings, arrays, and TypedArrays.
     */
    vm.slice_subseq = (sliceable, start, end = undefined) =>
    {
        vm.assert_type(start, "number");
        if (start > sliceable.length) throw new vm.Out_of_bounds_error();
        if (end === undefined) {
            return sliceable.slice(start);
        } else {
            if (end > sliceable.length) throw new vm.Out_of_bounds_error();
            return sliceable.slice(start, end);
        }
    };

    /*
     * Produces an option (one-element list) holding the object.
     */
    vm.some = (object) =>
    {
        return vm.list(object);
    };

    /*
     * Extract the contents of an option, or if it is nil, call a
     * thunk to obtain a default value.  If no thunk is specified,
     * produce void.
     */
    vm.optional = (option, default_thunk = () => vm.void()) =>
    {
        if (option === vm.nil())
            return default_thunk();
        else
            return vm.elt(option, 0);
    };

    /*
     * Signalled when an indexing operation is out of bounds.
     */
    vm.Out_of_bounds_error = class Lisp_out_of_bounds_error extends vm.Error
    {
        constructor()
        {
            super("Out of bounds");
        }
    };

    /*
     * Utility for turning the end argument to Lisp SUBSEQ into a JS
     * number or undefined.
     */
    function canonicalize_end(end) {
        if (end === vm.void()) return undefined;
        else return vm.assert_type(end, vm.Number).to_js_number();
    }

    vm.define_alien_function("%%list*", (...objects) => vm.list_star(...objects));

    vm.define_alien_function("%%append", (list1, list2) => vm.append(list1, list2));

    vm.define_alien_function("%%list-length", (list) => vm.num(vm.list_length(list)));

    vm.define_alien_function("%%nth", (num, list) =>
        vm.elt(list, vm.assert_type(num, vm.Number).to_js_number()));

    vm.define_alien_function("%%nthcdr", (num, list) =>
        vm.nthcdr(vm.assert_type(num, vm.Number).to_js_number(), list));

    vm.define_alien_function("%%list-subseq", (list, start, end) =>
        vm.list_subseq(list,
                       vm.assert_type(start, vm.Number).to_js_number(),
                       canonicalize_end(end)));

    vm.define_alien_function("%%string-subseq", (string, start, end) =>
        vm.string_subseq(string,
                         vm.assert_type(start, vm.Number).to_js_number(),
                         canonicalize_end(end)));

    vm.define_alien_function("%%reverse", (list) => vm.reverse(list));

    vm.define_alien_function("%%some", (value) => vm.some(value));

    vm.define_condition("out-of-bounds-error", vm.Out_of_bounds_error, vm.Error);

};


/***/ }),

/***/ "./src/stream.mjs":
/*!************************!*\
  !*** ./src/stream.mjs ***!
  \************************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "init_stream": () => (/* binding */ init_stream)
/* harmony export */ });
/*
 * LispX Input and Output Streams
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds stream-related functionality to a VM.
 */
function init_stream(vm)
{
    /*** Input Streams ***/

    /*
     * Abstract superclass of byte input streams.
     *
     * The API is inspired by Common Lisp's READ-BYTE and UNREAD-BYTE.
     *
     * Bytes are represented as JS strings with one code unit.
     */
    vm.Input_stream = class Lisp_input_stream extends vm.Object
    {
        /*
         * Attempts to read one byte from the stream.
         *
         * If no more bytes are available, and eof_error_p is true,
         * throws an error.  If false, returns eof_value.
         *
         * eof_error_p defaults to true.  eof_value defaults to #VOID.
         */
        read_byte(eof_error_p, eof_value) { vm.abstract_method(); }

        /*
         * Puts the most recently read byte back into the stream.
         *
         * The rules surrounding unreading aren't clearly defined at
         * the moment (see Common Lisp's UNREAD-BYTE for inspiration).
         * To stay on the safe side, always put back only one byte.
         */
        unread_byte(b) { vm.abstract_method(); }

        /*
         * Obtains the next byte in the stream without actually
         * reading it, thus leaving the byte to be read at a
         * later time.
         *
         * If peek_type is true, skips over whitespace.  If false,
         * not.
         */
        peek_byte(peek_type = false, eof_error_p = true, eof_value = vm.void())
        {
            const b = peek_type ? vm.skip_whitespace(this, false) : this.read_byte(false);
            if (b === vm.void()) {
                return vm.eof(eof_error_p, eof_value);
            } else {
                this.unread_byte(b);
                return b;
            }
        }
    };

    /*
     * Input stream that reads UTF-8 bytes from a string in memory.
     */
    vm.String_input_stream = class Lisp_string_input_stream extends vm.Input_stream
    {
        /*
         * Constructs a new string input stream from a Lisp string.
         */
        constructor(string)
        {
            super();
            vm.assert_type(string, vm.String);
            this.bytes = string.get_utf8_bytes();
            this.pos = -1;
        }

        /*
         * See Input_stream.
         */
        read_byte(eof_error_p = true, eof_value = vm.void())
        {
            vm.assert_type(eof_error_p, "boolean");
            vm.assert_type(eof_value, vm.TYPE_ANY);
            if ((this.pos + 1) < this.bytes.length) {
                this.pos++;
                return this.byte_at_pos(this.pos);
            } else {
                return vm.eof(eof_error_p, eof_value);
            }
        }

        /*
         * See Input_stream.
         */
        unread_byte(b)
        {
            vm.assert_type(b, "string");
            if ((this.pos >= 0) && (this.byte_at_pos(this.pos) === b))
                this.pos--;
            else
                throw new vm.Stream_error("Cannot unread byte");
        }

        /*
         * Returns the byte at the given position as a JS string.
         */
        byte_at_pos(pos)
        {
            return this.bytes[pos];
        }
    };

    /*** Output Streams ***/

    /*
     * Abstract superclass of byte output streams.
     *
     * Bytes are represented as JS strings with one code unit.
     */
    vm.Output_stream = class Lisp_output_stream extends vm.Object
    {
        /*
         * Attempts to write one byte to the stream.
         */
        write_byte(b) { vm.abstract_method(); }

        /*
         * Attempts to write the bytes from a string to the stream.
         */
        write_string(str)
        {
            vm.assert_type(str, vm.String);
            const bytes = str.get_utf8_bytes();
            for (let i = 0; i < bytes.length; i++)
                this.write_byte(bytes[i]);
            return str;
        }

        /*
         * Initiates the emptying of any internal buffers but does not
         * wait for completion or acknowledgment to return.
         */
        force_output()
        {
            /* Default implementation does nothing. */
            return vm.void();
        }

        /*
         * Ensure that the following output appears on a new line by
         * itself.  Return true if a newline character was printed,
         * false otherwise.
         */
        fresh_line()
        {
            /* Default implementation always writes newline. */
            this.write_byte("\n");
            return vm.t();
        }
    };

    /*
     * Output stream that writes UTF-8 bytes to a string in memory.
     */
    vm.String_output_stream = class Lisp_string_output_stream extends vm.Output_stream
    {
        /*
         * Constructs a new empty string output stream.
         */
        constructor()
        {
            super();
            this.bytes = "";
        }

        /*
         * Attempts to write one byte to the stream.
         */
        write_byte(b)
        {
            vm.assert_type(b, "string");
            vm.assert(b.length === 1);
            this.bytes += b;
            return b;
        }

        /*
         * Get the current stream contents as a string.
         */
        get_string()
        {
            return new vm.String(this.bytes);
        }
    };

    /*** Errors ***/

    /*
     * Throws an EOF error or returns the eof_value, depending
     * on eof_error_p.
     */
    vm.eof = (eof_error_p, eof_value) =>
    {
        if (eof_error_p)
            throw new vm.End_of_file();
        else
            return eof_value;
    };

    /*
     * Signalled when a stream-related error occurs.
     */
    vm.Stream_error = class Lisp_stream_error extends vm.Error
    {
        constructor(message)
        {
            super(message);
        }
    };

    /*
     * Signalled on EOF.
     */
    vm.End_of_file = class Lisp_end_of_file extends vm.Stream_error
    {
        constructor()
        {
            super("EOF");
        }
    };

    /*** Standard Streams ***/

    vm.STANDARD_INPUT = vm.make_dynamic(vm.void());

    vm.STANDARD_OUTPUT = vm.make_dynamic(vm.void());

    /*** Lisp API ***/

    vm.define_class("input-stream", vm.Input_stream);

    vm.define_class("string-input-stream", vm.String_input_stream, vm.Input_stream);

    vm.define_class("output-stream", vm.Output_stream);

    vm.define_class("string-output-stream", vm.String_output_stream, vm.Output_stream);

    vm.define_condition("stream-error", vm.Stream_error, vm.Error);

    vm.define_condition("end-of-file", vm.End_of_file, vm.Stream_error);

    vm.define_variable("*standard-input*", vm.STANDARD_INPUT);

    vm.define_variable("*standard-output*", vm.STANDARD_OUTPUT);

    vm.define_alien_function("%%make-string-input-stream", (string) =>
        new vm.String_input_stream(string));

    vm.define_alien_function("%%make-string-output-stream", () =>
        new vm.String_output_stream());

    vm.define_alien_function("%%get-output-stream-string", (sos) =>
        vm.assert_type(sos, vm.String_output_stream).get_string());

    vm.define_alien_function("%%fresh-line", (stream) =>
        vm.assert_type(stream, vm.Output_stream).fresh_line());

};


/***/ })

/******/ });
/************************************************************************/
/******/ // The module cache
/******/ var __webpack_module_cache__ = {};
/******/ 
/******/ // The require function
/******/ function __webpack_require__(moduleId) {
/******/ 	// Check if module is in cache
/******/ 	var cachedModule = __webpack_module_cache__[moduleId];
/******/ 	if (cachedModule !== undefined) {
/******/ 		return cachedModule.exports;
/******/ 	}
/******/ 	// Create a new module (and put it into the cache)
/******/ 	var module = __webpack_module_cache__[moduleId] = {
/******/ 		// no module.id needed
/******/ 		// no module.loaded needed
/******/ 		exports: {}
/******/ 	};
/******/ 
/******/ 	// Execute the module function
/******/ 	__webpack_modules__[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/ 
/******/ 	// Return the exports of the module
/******/ 	return module.exports;
/******/ }
/******/ 
/************************************************************************/
/******/ /* webpack/runtime/define property getters */
/******/ (() => {
/******/ 	// define getter functions for harmony exports
/******/ 	__webpack_require__.d = (exports, definition) => {
/******/ 		for(var key in definition) {
/******/ 			if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 				Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 			}
/******/ 		}
/******/ 	};
/******/ })();
/******/ 
/******/ /* webpack/runtime/hasOwnProperty shorthand */
/******/ (() => {
/******/ 	__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
/******/ })();
/******/ 
/******/ /* webpack/runtime/make namespace object */
/******/ (() => {
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = (exports) => {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/ })();
/******/ 
/************************************************************************/
var __webpack_exports__ = {};
// This entry need to be wrapped in an IIFE because it need to be isolated against other modules in the chunk.
(() => {
/*!********************!*\
  !*** ./src/vm.mjs ***!
  \********************/
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "VM": () => (/* binding */ VM)
/* harmony export */ });
/* harmony import */ var big_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! big.js */ "./node_modules/big.js/big.js");
/* harmony import */ var _eval_mjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./eval.mjs */ "./src/eval.mjs");
/* harmony import */ var _control_mjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./control.mjs */ "./src/control.mjs");
/* harmony import */ var _seq_mjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./seq.mjs */ "./src/seq.mjs");
/* harmony import */ var _stream_mjs__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./stream.mjs */ "./src/stream.mjs");
/* harmony import */ var _read_mjs__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ./read.mjs */ "./src/read.mjs");
/* harmony import */ var _print_mjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ./print.mjs */ "./src/print.mjs");
/* harmony import */ var _js_mjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ./js.mjs */ "./src/js.mjs");
/* harmony import */ var _boot_lispx__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ./boot.lispx */ "./src/boot.lispx");
/* harmony import */ var _stream_lispx__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! ./stream.lispx */ "./src/stream.lispx");
/* harmony import */ var _read_lispx__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! ./read.lispx */ "./src/read.lispx");
/* harmony import */ var _print_lispx__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! ./print.lispx */ "./src/print.lispx");
/* harmony import */ var _js_lispx__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! ./js.lispx */ "./src/js.lispx");
/*
 * LispX Virtual Machine
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Arbitrary-precision decimals.
 */


/*
 * Import VM modules.
 *
 * Currently, all modules are always loaded, but in the future it will
 * be possible to make them optional, so that one can obtain e.g. a
 * smaller VM without IO support.
 */








/*
 * A build system contraption loads the contents of the files into the
 * variables as strings.
 */






/*
 * A virtual machine is a Lisp interpreter.
 *
 * Multiple independent VMs can exist in the same JavaScript
 * program.
 */
class VM
{
    /*
     * Creates a new VM.
     */
    constructor()
    {
        /*
         * Set up class hierarchy and other internal data structures.
         */
        init_vm(this);

        /*
         * Load modules.
         */
        (0,_eval_mjs__WEBPACK_IMPORTED_MODULE_1__.init_eval)(this);
        (0,_control_mjs__WEBPACK_IMPORTED_MODULE_2__.init_control)(this);
        (0,_seq_mjs__WEBPACK_IMPORTED_MODULE_3__.init_seq)(this);
        (0,_stream_mjs__WEBPACK_IMPORTED_MODULE_4__.init_stream)(this);
        (0,_read_mjs__WEBPACK_IMPORTED_MODULE_5__.init_read)(this);
        (0,_print_mjs__WEBPACK_IMPORTED_MODULE_6__.init_print)(this);
        (0,_js_mjs__WEBPACK_IMPORTED_MODULE_7__.init_js)(this);

        /*
         * Evaluate the bootstrap code.
         */
        this.eval_js_string(_boot_lispx__WEBPACK_IMPORTED_MODULE_8__);
        this.eval_js_string(_stream_lispx__WEBPACK_IMPORTED_MODULE_9__);
        this.eval_js_string(_read_lispx__WEBPACK_IMPORTED_MODULE_10__);
        this.eval_js_string(_print_lispx__WEBPACK_IMPORTED_MODULE_11__);
        this.eval_js_string(_js_lispx__WEBPACK_IMPORTED_MODULE_12__);
    }

    /*
     * Creates a Lisp string from a JavaScript string.
     */
    str(js_string)
    {
        return new this.String(this.utf8_encode(js_string));
    }

    /*
     * Gets or creates an interned symbol from a JavaScript string.
     *
     * The symbol's namespace defaults to the variable namespace.
     */
    sym(js_string, namespace = this.VARIABLE_NAMESPACE)
    {
        return this.intern(this.str(js_string), namespace);
    }

    /*
     * Gets or creates an interned function symbol from a JavaScript string.
     */
    fsym(js_string)
    {
        return this.sym(js_string, this.FUNCTION_NAMESPACE);
    }

    /*
     * Gets or creates an interned class symbol from a JavaScript string.
     */
    csym(js_string)
    {
        return this.sym(js_string, this.CLASS_NAMESPACE);
    }

    /*
     * Gets or creates an interned keyword symbol from a JS string.
     */
    kwd(js_string)
    {
        return this.sym(js_string, this.KEYWORD_NAMESPACE);
    }

    /*
     * Gets or creates an interned symbol from a Lisp string.
     *
     * The symbol's namespace defaults to the variable namespace.
     */
    intern(string, namespace = this.VARIABLE_NAMESPACE)
    {
        this.assert_type(string, this.String);
        this.assert_type(namespace, "string");
        const key = this.Symbol.make_key(string, namespace);
        const sym = this.symbols[key];
        if (sym !== undefined)
            return sym;
        else
            return this.symbols[key] = new this.Symbol(string, namespace);
    }

    /*
     * Creates a new number from a JS string or number.
     */
    num(js_string_or_number)
    {
        this.assert_type(js_string_or_number, this.type_or("string", "number"));
        return new this.Number(new big_js__WEBPACK_IMPORTED_MODULE_0__(js_string_or_number));
    }

    /*
     * Constructs a new pair.
     */
    cons(car, cdr)
    {
        return new this.Cons(car, cdr);
    }

    /*
     * Constructs a new list from its arguments.
     */
    list(...objects)
    {
        return this.array_to_list(objects);
    }

    /*
     * Creates a new environment.
     *
     * If the parent environment is non-null, the new environment
     * will inherit bindings from it.
     */
    make_environment(parent = null)
    {
        return new this.Environment(parent);
    }

    /*
     * Value equality.
     *
     * Lisp classes can implement the equal_same_type() method to hook
     * into this.
     *
     * For non-Lisp objects, or Lisp objects of different classes,
     * it uses ===.
     */
    equal(a, b)
    {
        if (this.is_lisp_object(a) && this.is_lisp_object(b))
            if (this.class_of(a) === this.class_of(b))
                return a.equal_same_type(b);
        return a === b;
    }

    /*
     * Ordering.  Returns a value less than 0, equal to 0, or greater
     * than 0 depending on whether a is smaller than, equal to, or
     * larger than b.
     *
     * Lisp classes can implement the compare_same_type() method to hook
     * into this.
     *
     * For non-Lisp objects, or Lisp objects of different classes,
     * it throws an error.
     */
    compare(a, b)
    {
        if (this.is_lisp_object(a) && this.is_lisp_object(b))
            if (this.class_of(a) === this.class_of(b))
                return a.compare_same_type(b);
        throw this.make_type_error(b, this.class_of(a).get_js_class());
    }

    /*
     * Returns true if an object is a Lisp object, false if it is a
     * JavaScript object.
     */
    is_lisp_object(obj)
    {
        return obj instanceof this.Object;
    }

    /*
     * Returns the Lisp class (metaobject) of an object.
     *
     * For non-Lisp objects, returns OBJECT, so this can be called on
     * any object.
     */
    class_of(obj)
    {
        if (this.is_lisp_object(obj))
            return obj.lisp_class;
        else
            return this.lisp_class(this.Object);
    }

    /*
     * Returns the Lisp class (metaobject) of a JS class.
     */
    lisp_class(js_class)
    {
        this.assert_type(js_class, "function");
        this.assert(this.has_lisp_class(js_class));
        return js_class.prototype.lisp_class;
    }

    /*
     * Returns true if a JS class has a Lisp class metaobject, false
     * otherwise.  This can be used to discover whether the class
     * belongs to the VM or is an unrelated JS class.
     */
    has_lisp_class(js_class)
    {
        return this.has_type(js_class.prototype.lisp_class, this.Class);
    }

    /*
     * Returns the VM's root environment;
     */
    get_environment()
    {
        return this.environment;
    }
}

/*
 * Initializes a virtual machine.
 */
function init_vm(vm)
{
    /*** Objects ***/

    /*
     * The root class of the class hierarchy.
     */
    vm.Object = class Lisp_object
    {
        /*
         * Classes can override this method to provider value equality
         * semantics.
         *
         * Don't call directly, use vm.equal() instead.
         *
         * Other is guaranteed to have the same class as this object.
         *
         * Defaults to reference equality (===).
         */
        equal_same_type(other)
        {
            return this === other;
        }

        /*
         * Classes can override this method to provider ordering
         * semantics.
         *
         * Don't call directly, use vm.compare() instead.
         *
         * Other is guaranteed to have the same class as this object.
         *
         * Throws an error by default.
         */
        compare_same_type(other)
        {
            vm.abstract_method();
        }
    };

    /*
     * UTF-8 string.
     */
    vm.String = class Lisp_string extends vm.Object
    {
        /*
         * Creates a new Lisp string from UTF-8 bytes encoded as
         * UTF-16 code units.
         *
         * Use vm.str() instead unless you know what you are doing.
         */
        constructor(utf8_bytes)
        {
            super();
            vm.assert_type(utf8_bytes, "string");
            this.utf8_bytes = utf8_bytes;
        }

        /*
         * Strings are equal if their UTF-8 bytes are equal.
         */
        equal_same_type(other)
        {
            return this.get_utf8_bytes() === other.get_utf8_bytes();
        }

        /*
         * Creates a UTF-16 JavaScript string from a Lisp string.
         */
        to_js_string()
        {
            return vm.utf8_decode(this.get_utf8_bytes());
        }

        /*
         * Returns the UTF-8 bytes of a Lisp string, encoded as UTF-16
         * code units.
         */
        get_utf8_bytes()
        {
            return this.utf8_bytes;
        }
    };

    /*
     * Takes a string containing UTF-16 code units and returns a string in
     * which every code unit represents a UTF-8 byte.
     */
    vm.utf8_encode = (js_string) =>
    {
        vm.assert_type(js_string, "string");
        return unescape(encodeURIComponent(js_string));
    };

    /*
     * Takes a string containing UTF-8 bytes encoded as UTF-16 code units,
     * and returns a bona fide UTF-16 string.
     */
    vm.utf8_decode = (bytes) =>
    {
        vm.assert_type(bytes, "string");
        return decodeURIComponent(escape(bytes));
    };

    /*
     * Symbol, an identifier.
     *
     * A symbol has a name, which is a string, and a namespace.
     *
     * The namespaces are variable, function, class, and keyword.
     *
     * Namespaces allow us to have objects of different type with the
     * same name.  For example, there can be an ordinary variable,
     * a function, and a class called LIST in the same environment.
     *
     * The keyword namespace is slightly different: symbols in it
     * evaluate to themselves.
     */
    vm.Symbol = class Lisp_symbol extends vm.Object
    {
        /*
         * Creates a new, uninterned symbol with a string as name, and
         * a namespace.
         *
         * Do not use this directly, always use vm.sym(), as uninterned
         * symbols are not currently fully supported. (In the common
         * Lisp semantics, uninterned symbols can be used e.g. as variable
         * names, and will be distinct from each other, even if they have
         * the same name.  Our current implementation does not support this,
         * however, and uses symbol names as keys, so distinct uninterned
         * symbols with the same name would clash.  The benefit of this is
         * easy introspectability of environments in the JS developer tools.)
         */
        constructor(string, namespace)
        {
            super();
            this.string = vm.assert_type(string, vm.String);
            this.namespace = vm.assert_type(namespace, "string");
        }

        /*
         * Returns the name string of the symbol.
         */
        get_string()
        {
            return this.string;
        }

        /*
         * Returns the namespace of the symbol.
         */
        get_namespace()
        {
            return this.namespace;
        }

        /*
         * Return the symbol with the same name in another namespace.
         *
         * Prefer one of the to_x_symbol() methods, below.
         */
        to_namespace(namespace)
        {
            vm.assert_type(namespace, "string");
            return vm.intern(this.get_string(), namespace);
        }

        /*
         * Return the symbol with the same name in the variable namespace.
         */
        to_variable_symbol()
        {
            return this.to_namespace(vm.VARIABLE_NAMESPACE);
        }

        /*
         * Return the symbol with the same name in the function namespace.
         */
        to_function_symbol()
        {
            return this.to_namespace(vm.FUNCTION_NAMESPACE);
        }

        /*
         * Return the symbol with the same name in the class namespace.
         */
        to_class_symbol()
        {
            return this.to_namespace(vm.CLASS_NAMESPACE);
        }

        /*
         * Return the symbol with the same name in the keyword namespace.
         */
        to_keyword_symbol()
        {
            return this.to_namespace(vm.KEYWORD_NAMESPACE);
        }

        /*
         * The symbol key is a string under which the symbol is
         * stored in associative data structures like the symbol table
         * and environments.
         */
        get_key()
        {
            return vm.Symbol.make_key(this.get_string(), this.get_namespace());
        }

        /*
         * Utility method that creates a symbol key.
         */
        static make_key(string, namespace)
        {
            return string.get_utf8_bytes() + "_" + namespace;
        }
    }

    /*
     * The namespaces.
     */
    vm.VARIABLE_NAMESPACE = "variable";
    vm.FUNCTION_NAMESPACE = "function";
    vm.CLASS_NAMESPACE = "class";
    vm.KEYWORD_NAMESPACE = "keyword";

    /*
     * Arbitrary-precision decimal number.
     */
    vm.Number = class Lisp_number extends vm.Object
    {
        /*
         * Construct a new number from a Big.
         *
         * Use vm.num() instead of calling this directly.
         */
        constructor(big)
        {
            super();
            vm.assert_type(big, big_js__WEBPACK_IMPORTED_MODULE_0__);
            this.big = big;
        }

        /*
         * Numbers are equal if their underlying Bigs are equal.
         */
        equal_same_type(other)
        {
            return this.get_big().eq(other.get_big());
        }

        /*
         * Numbers compare by comparing their underlying Bigs.
         */
        compare_same_type(other)
        {
            return this.get_big().cmp(other.get_big());
        }

        /*
         * Transforms a Lisp number into a JS one.
         */
        to_js_number()
        {
            return this.get_big().toNumber();
        }

        /*
         * Transforms a Lisp number into a string.
         */
        to_string()
        {
            return vm.str(this.get_big().toFixed());
        }

        /*
         * Internal accessor.
         */
        get_big() { return this.big; }
    }

    vm.add = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().add(b.get_big()));
    };

    vm.subtract = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().minus(b.get_big()));
    };

    vm.multiply = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().times(b.get_big()));
    };

    vm.divide = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().div(b.get_big()));
    };

    /*
     * Mr. Boole's fabulous invention.
     */
    vm.Boolean = class Lisp_boolean extends vm.Object
    {
        /*
         * Construct a new boolean from a JS boolean.
         *
         * Use vm.t() or vm.f() instead of calling this.
         */
        constructor(js_bool)
        {
            super();
            this.bool = vm.assert_type(js_bool, "boolean");
        }

        /*
         * Transforms a Lisp boolean into a JS one.
         */
        to_js_boolean()
        {
            return this.bool;
        }
    }

    /*
     * Superclass of conses and #NIL.
     */
    vm.List = class Lisp_list extends vm.Object {};

    /*
     * A pair of two values, the most fundamental data structure.
     */
    vm.Cons = class Lisp_cons extends vm.List
    {
        /*
         * Construct a new cons.
         *
         * Use vm.cons() instead of calling this.
         */
        constructor(car, cdr)
        {
            super();
            this._car = car;
            this._cdr = cdr;
        }

        /*
         * Conses are equal if their car and cdr are equal.
         */
        equal_same_type(other)
        {
            return vm.equal(this.car(), other.car()) && vm.equal(this.cdr(), other.cdr());
        }

        /*
         * Contents of the address part of the register.
         */
        car() { return this._car; }
        set_car(car) { this._car = car; }

        /*
         * Contents of the decrement part of the register.
         */
        cdr() { return this._cdr; }
        set_cdr(cdr) { this._cdr = cdr; }
    }

    /*
     * #NIL, the empty list.
     */
    vm.Nil = class Lisp_nil extends vm.List
    {
        /*
         * Construct #NIL.
         *
         * Use vm.nil() instead of calling this.
         */
        constructor()
        {
            super();
        }
    }

    /*
     * #VOID, the value that is used to indicate that no
     * interesting value was produced, e.g. by an empty (PROGN).
     */
    vm.Void = class Lisp_void extends vm.Object
    {
        /*
         * Construct #VOID.
         *
         * Use vm.void() instead of calling this.
         */
        constructor()
        {
            super();
        }
    }

    /*
     * #IGNORE, used on the left hand side of a definition
     * if no binding is desired.
     */
    vm.Ignore = class Lisp_ignore extends vm.Object
    {
        /*
         * Construct #IGNORE.
         *
         * Use vm.ignore() instead of calling this.
         */
        constructor()
        {
            super();
        }
    }

    /*
     * A lexical environment maps symbols to values.
     *
     * It may have a parent environment, in which bindings are looked
     * up if they are not found.
     */
    vm.Environment = class Lisp_environment extends vm.Object
    {
        /*
         * Creates an environment with an optional parent.
         *
         * The bindings object inherits from the parent's bindings,
         * if it exists.
         *
         * Use vm.make_environment() instead of calling this directly.
         */
        constructor(parent = null)
        {
            super();
            if (parent !== null)
                vm.assert_type(parent, vm.Environment);
            this.parent = parent;
            this.bindings = Object.create(
                (parent === null) ? null : parent.get_bindings()
            );
        }

        /*
         * Assigns a value to a symbol in this environment.
         */
        put(symbol, value)
        {
            vm.assert_type(symbol, vm.Symbol);
            this.bindings[symbol.get_key()] = value;
        }

        /*
         * Returns the value of a symbol in this environment,
         * or one of its parent environments.
         *
         * Throws an error if the symbol is not bound.
         */
        lookup(symbol)
        {
            /*
             * There are some mild shenanigans ahead because we want to
             * achieve a trifecta of objectives:
             *
             * - Environments should be able to store any JS value
             *   whatsoever, including undefined.
             *
             * - Accessing an unbound variable should throw (and not return
             *   undefined as in JS).
             *
             * - Lookup should use JS's optimized code for looking up
             *   things in prototype hierarchies.
             *
             * Optimize for the common case that the symbol exists and
             * is not bound to undefined.
             */
            vm.assert_type(symbol, vm.Symbol);
            const key = symbol.get_key();
            const val = this.bindings[key];
            if (val !== undefined) {
                return val;
            } else {
                /*
                 * We got undefined, so now we need to check whether
                 * the symbol is bound to undefined, or doesn't exist.
                 */
                if (key in this.bindings) {
                    /*
                     * The symbol exists, so its value is indeed undefined.
                     */
                    return undefined;
                } else {
                    /*
                     * The symbol is unbound.
                     */
                    throw new vm.Unbound_symbol_error(symbol, this);
                }
            }
        }

        /*
         * Returns true if a symbol is bound in this environment
         * or one of its ancestors, false otherwise.
         */
        is_bound(symbol)
        {
            vm.assert_type(symbol, vm.Symbol);
            const key = symbol.get_key();
            return key in this.bindings;
        }

        /*
         * Internal accessor.
         */
        get_bindings() { return this.bindings; }
    }

    /*** Classes ***/

    /*
     * Superclass of all class metaobjects.
     *
     * This class is abstract; all concrete classes are instances of
     * either BUILT-IN-CLASS or STANDARD-CLASS.
     */
    vm.Class = class Lisp_class extends vm.Object
    {
        /*
         * Constructs a new class with the given name, JS constructor
         * function, and superclass.  Superclass can be null, but only
         * in case of the root of the class hierarchy, OBJECT.
         */
        constructor(name, js_class, superclass)
        {
            super();
            this.name = vm.assert_type(name, vm.Symbol);
            this.js_class = vm.assert_type(js_class, "function");
            this.superclass = vm.assert_type(superclass, vm.type_or(vm.TYPE_NULL, vm.Class));
        }

        /*
         * Creates or updates a method in this class.
         */
        put_method(name, method)
        {
            vm.assert_type(method, vm.Operator);
            const key = this.method_key(name);
            this.get_js_class().prototype[key] = method;
        }

        /*
         * Searches for a method in this class or a superclass.
         *
         * Throws an error if the method is not found.
         */
        lookup_method(object, name)
        {
            const key = this.method_key(name);
            /*
             * Looking up the method in the class, and not directly in
             * the object itself, makes this also work for JS objects
             * like null.
             */
            const method = this.get_js_class().prototype[key];
            if (method !== undefined)
                return method;
            else
                throw new vm.Unbound_method_error(object, name);
        }

        /*
         * Internal method that constructs the key under which a
         * method is stored in the JS class.
         */
        method_key(method_name)
        {
            vm.assert_type(method_name, vm.Symbol);
            const bytes = method_name.get_string().get_utf8_bytes();
            return "lisp_method_" + bytes;
        }

        /*
         * Accessors.
         */
        get_name() { return this.name; }
        get_js_class() { return this.js_class; }
        get_superclass() { return this.superclass; }
    };

    /*
     * Metaclass of built-in classes like STRING and CONS.
     */
    vm.Built_in_class = class Lisp_built_in_class extends vm.Class
    {
        constructor(name, js_class, superclass)
        {
            super(name, js_class, superclass);
        }
    };

    /*
     * Metaclass of classes defined with DEFCLASS by the user.
     */
    vm.Standard_class = class Lisp_standard_class extends vm.Class
    {
        constructor(name, js_class, superclass)
        {
            super(name, js_class, superclass);
        }
    };

    /*
     * Superclass of classes defined with DEFCLASS by the user.
     */
    vm.Standard_object = class Lisp_standard_object extends vm.Object
    {
        /*
         * Retrieves the value of a slot of a standard object.
         */
        slot_value(slot_name)
        {
            const slot_key = this.slot_key(slot_name);
            if (this.hasOwnProperty(slot_key))
                return this[slot_key];
            else
                throw new vm.Unbound_slot_error(this, slot_name);
        }

        /*
         * Updates the value of a slot of a standard object.
         */
        set_slot_value(slot_name, slot_value)
        {
            const slot_key = this.slot_key(slot_name);
            return this[slot_key] = slot_value;
        }

        /*
         * Returns true if a slot is bound in this object,
         * false otherwise.
         */
        is_slot_bound(slot_name)
        {
            const slot_key = this.slot_key(slot_name);
            return slot_key in this;
        }

        /*
         * Internal method that constructs the key under
         * which a slot value is stored in the object.
         */
        slot_key(slot_name)
        {
            vm.assert_type(slot_name, vm.Symbol);
            const bytes = slot_name.get_string().get_utf8_bytes();
            return "lisp_slot_" + bytes;
        }
    };

    /*
     * Creates an instance of a standard class.
     *
     * Slots are initialized from the slot initializers, an array of
     * even length, alternately containing slot names and values.
     */
    vm.make_instance = (cls, ...slot_inits) =>
    {
        vm.assert_type(cls, vm.Standard_class);
        vm.assert((slot_inits.length % 2) === 0);
        const obj = new (cls.get_js_class())();
        for (let i = 0; i < slot_inits.length; i = i + 2) {
            const name = slot_inits[i];
            const value = slot_inits[i + 1];
            obj.set_slot_value(name, value);
        }
        return obj;
    };

    /*
     * Check if a class is a subclass of another class.
     *
     * A class is considered a subclass of itself.
     */
    vm.is_subclass = (sub_class, super_class) =>
    {
        vm.assert_type(sub_class, vm.Class);
        vm.assert_type(super_class, vm.Class);
        if (sub_class === super_class) {
            return true;
        } else {
            const js_sub_class = sub_class.get_js_class();
            const js_super_class = super_class.get_js_class();
            return js_sub_class.prototype instanceof js_super_class;
        }
    };

    /*
     * Creates a new standard class dynamically.
     */
    vm.make_standard_class = (name, lisp_super) =>
    {
        vm.assert_type(name, vm.Symbol);
        vm.assert_type(lisp_super, vm.Standard_class);
        const js_super = lisp_super.get_js_class();
        const js_class = class extends js_super
        {
            constructor()
            {
                super();
            }
        };
        const js_name = name.get_string().to_js_string();
        Object.defineProperty(js_class, "name", { value: js_name });
        const lisp_class = vm.bless_class(name, js_class, js_super, vm.Standard_class);
        return lisp_class;
    };

    /*** Conditions ***/

    /*
     * Superclass of all objects that are signalled by Lisp.
     */
    vm.Condition = class Lisp_condition extends vm.Standard_object {};

    /*
     * Superclass of all Lisp errors.
     */
    vm.Error = class Lisp_error extends vm.Condition
    {
        constructor(message)
        {
            super();

            /*
             * Kludge to get a stack trace even though
             * we're not extending JS's Error.
             */
            const error = new Error(message);
            Object.getOwnPropertyNames(error)
                .forEach((name) => {
                    const desc = Object.getOwnPropertyDescriptor(error, name);
                    Object.defineProperty(this, name, desc);
                });
        }
    };

    /*
     * Signalled when an object doesn't match an expected type.
     *
     * The expected type should be a symbolic Lisp type specifier.
     *
     * See make_type_error().
     */
    vm.Type_error = class Lisp_type_error extends vm.Error
    {
        constructor(datum, expected_type)
        {
            super("Type assertion failed: expected " + vm.write_to_js_string(expected_type));
            this.lisp_slot_datum = datum;
            this["lisp_slot_expected-type"] = expected_type;
        }
    };

    /*
     * Creates a new type error and converts the expected type to a
     * symbolic Lisp type specifier.
     */
    vm.make_type_error = (datum, expected_type) =>
    {
        return new vm.Type_error(datum, vm.to_lisp_type_spec(expected_type));
    };

    /*
     * Signalled when a symbol cannot be found in an environment.
     */
    vm.Unbound_symbol_error = class Lisp_unbound_symbol_error extends vm.Error
    {
        constructor(symbol, env)
        {
            vm.assert_type(symbol, vm.Symbol);
            vm.assert_type(env, vm.Environment);

            const name = symbol.get_string().to_js_string();
            const namespace = symbol.get_namespace();
            super(`Unbound ${namespace}: ${name}`);

            this.lisp_slot_symbol = symbol;
            this.lisp_slot_environment = env;
        }
    };

    /*
     * Signalled when a slot cannot be found in a standard object.
     */
    vm.Unbound_slot_error = class Lisp_unbound_slot_error extends vm.Error
    {
        constructor(obj, slot_name)
        {
            vm.assert_type(obj, vm.Standard_object);
            vm.assert_type(slot_name, vm.Symbol);

            const name = slot_name.get_string().to_js_string();
            super(`Unbound slot: ${name}`);

            this.lisp_slot_object = obj;
            this["lisp_slot_slot-name"] = slot_name;
        }
    };

    /*
     * Signalled when a method cannot be found in an object.
     */
    vm.Unbound_method_error = class Lisp_unbound_method_error extends vm.Error
    {
        constructor(obj, method_name)
        {
            vm.assert_type(method_name, vm.Symbol);

            const name = method_name.get_string().to_js_string();
            super(`Unbound method: ${name}`);

            this.lisp_slot_object = obj;
            this["lisp_slot_method-name"] = method_name;
        }
    };

    /*
     * Signalled when an assertion failed.
     */
    vm.Assertion_error = class Lisp_assertion_error extends vm.Error
    {
        constructor(message)
        {
            super(message || "Assertion failed");
        }
    };

    /*
     * If the boolean is false, throws an error with a message.
     */
    vm.assert = (bool, message = "Assertion failed") =>
    {
        if (!bool) throw new vm.Assertion_error(message);
    };

    /*
     * Used in the bodies of abstract methods.
     */
    vm.abstract_method = () =>
    {
        throw new vm.Error("You called an abstract method. Congratulations!");
    };

    /*** Type Specs ***/

    /*
     * We use a system of type specifiers to check the types of
     * arguments passed to JS functions.
     *
     * Type specs can also be translated to symbolic Lisp data
     * for error reporting.
     */

    /*
     * Returns true if a datum matches a type spec, false otherwise.
     *
     * A type spec can be a:
     *
     * - String: in this case, the datum's typeof must be equal to the
     *   type spec.
     *
     * - Function: the type spec must be the JS class of a Lisp class
     *   and the datum must be instanceof the type spec.
     *
     * - Object with a custom_check method: the custom_check methods gets
     *   called to determine whether the datum matches.
     */
    vm.has_type = (datum, type_spec) =>
    {
        if (typeof(type_spec) === "string") {
            return typeof(datum) === type_spec;
        } else if (typeof(type_spec) === "function") {
            return datum instanceof type_spec;
        } else if (type_spec && type_spec.custom_check) {
            return type_spec.custom_check(datum);
        } else {
            throw new vm.Error("Unknown type spec");
        }
    };

    /*
     * A type spec that matches any datum.
     */
    vm.TYPE_ANY = {
        custom_check(datum)
        {
            return true;
        },
        to_lisp_type_spec()
        {
            return vm.sym("object");
        }
    };

    /*
     * A type spec that matches null.
     */
    vm.TYPE_NULL = {
        custom_check(datum)
        {
            return datum === null;
        },
        to_lisp_type_spec()
        {
            return vm.str("null");
        }
    };

    /*
     * A type spec that matches any of its constituent type specs.
     */
    vm.type_or = (...elements) =>
    {
        return {
            custom_check(datum)
            {
                for (let i = 0; i < elements.length; i++)
                    if (vm.has_type(datum, elements[i]))
                        return true;
                return false;
            },
            to_lisp_type_spec()
            {
                return vm.cons(vm.sym("or"),
                               vm.array_to_list(elements.map(vm.to_lisp_type_spec)));
            }
        }
    };

    /*
     * Checks that a datum matches a type spec with vm.has_type(),
     * and throws an error if it doesn't.  Returns the datum.
     */
    vm.assert_type = (datum, type_spec) =>
    {
        if (vm.has_type(datum, type_spec)) {
            return datum;
        } else {
            throw vm.make_type_error(datum, type_spec);
        }
    };

    /*
     * Transforms a type spec into a symbolic representation used on
     * the Lisp side.
     *
     * A symbolic Lisp type specifier is one of:
     *
     * - (Lisp) String: in this case the type specifier describes
     *   a JavaScript typeof check.  As a special case, "null" stands
     *   for the JS null value.
     *
     * - Symbol: the type specifier describes a Lisp class by name.
     *
     * - List: compound type specifier.  Currently supported are
     *   OR type specifiers of the form
     *   (or <type-spec-1> ... <type-spec-N>).
     */
    vm.to_lisp_type_spec = (type_spec) =>
    {
        if (typeof(type_spec) === "string") {
            return vm.str(type_spec);
        } else if (typeof(type_spec) === "function") {
            if (vm.has_lisp_class(type_spec))
                return vm.lisp_class(type_spec).get_name();
            else
                /*
                 * Temporary hack: we don't have a symbolic type spec
                 * format for non-VM, ordinary JS classes yet.  For
                 * now, we just use the class itself as its type spec.
                 */
                return type_spec;
        } else if (type_spec && type_spec.to_lisp_type_spec) {
            return type_spec.to_lisp_type_spec();
        } else {
            throw new vm.Error("Unknown type spec");
        }
    };

    /*** Lists ***/

    /*
     * Returns the list element at a given position.
     */
    vm.elt = (list, i) =>
    {
        vm.assert_type(list, vm.Cons);
        vm.assert_type(i, "number");
        if (i === 0)
            return list.car();
        else
            return vm.elt(list.cdr(), i - 1);
    };

    /*
     * Turns a list into a JS array.
     */
    vm.list_to_array = (list) =>
    {
        vm.assert_type(list, vm.List);
        const array = [];
        while (list !== vm.nil()) {
            array.push(list.car());
            list = list.cdr();
        }
        return array;
    };

    /*
     * Turns a JS array into a list.
     */
    vm.array_to_list = (array) =>
    {
        vm.assert(Array.isArray(array), "Not an array");
        let list = vm.nil();
        for (let i = array.length; i > 0; i--)
            list = vm.cons(array[i - 1], list);
        return list;
    };

    /*
     * Reverses a list.
     */
    vm.reverse = (list) =>
    {
        vm.assert_type(list, vm.List);
        let rev_list = vm.nil();
        while (list !== vm.nil()) {
            rev_list = vm.cons(list.car(), rev_list);
            list = list.cdr();
        }
        return rev_list;
    };

    /*** Internal VM Data Structures ***/

    /*
     * The root, or global, environment.
     */
    vm.environment = vm.make_environment();

    /*
     * Symbol table, maps symbol keys to interned symbols.
     */
    vm.symbols = Object.create(null);

    /*
     * The peanut gallery.
     */
    const T = new vm.Boolean(true);
    const F = new vm.Boolean(false);
    const NIL = new vm.Nil();
    const VOID = new vm.Void();
    const IGNORE = new vm.Ignore();
    const ZERO = vm.num(0);
    const ONE = vm.num(1);

    /*
     * Wrap them in functions so typos cause an error, and don't
     * result in undefined.
     */
    vm.t = () => T;
    vm.f = () => F;
    vm.nil = () => NIL;
    vm.void = () => VOID;
    vm.ignore = () => IGNORE;
    vm.zero = () => ZERO;
    vm.one = () => ONE;

    /*** Class Metaobjects ***/

    /*
     * Creates a class metaobject for a JS class and registers the
     * class in the root environment.
     *
     * Note that the class gets registered in the class namespace,
     * while its name property is an ordinary variable symbol.
     * We could have used the class namespace for both, but it's
     * more readable if class names are just ordinary symbols.
     */
    vm.define_class = (name, js_class, js_super = vm.Object, js_meta = vm.Built_in_class) =>
    {
        const name_sym = vm.sym(name);
        const lisp_class = vm.bless_class(name_sym, js_class, js_super, js_meta);
        vm.get_environment().put(name_sym.to_class_symbol(), lisp_class);
    };

    /*
     * Creates a class metaobject for a JS class.
     */
    vm.bless_class = (name_sym, js_class, js_super = vm.Object, js_meta = vm.Built_in_class) =>
    {
        vm.assert_type(name_sym, vm.Symbol);
        vm.assert_type(js_class, "function");
        vm.assert_type(js_super, vm.type_or(vm.TYPE_NULL, "function"));
        vm.assert_type(js_meta, "function");
        const lisp_super = js_super ? vm.lisp_class(js_super) : null;
        const lisp_class = new js_meta(name_sym, js_class, lisp_super);
        js_class.prototype.lisp_class = lisp_class;
        return lisp_class;
    };

    /*
     * Defines a condition class.  Used so that STANDARD-CLASS doesn't
     * have to be specified explicitly as metaclass.
     */
    vm.define_condition = (name, js_class, js_super) =>
    {
        vm.define_class(name, js_class, js_super, vm.Standard_class);
    };

    /*** Lisp API ***/

    vm.define_class("object", vm.Object, null);

    vm.define_class("string", vm.String);

    vm.define_class("symbol", vm.Symbol);

    vm.define_class("number", vm.Number);

    vm.define_class("boolean", vm.Boolean);

    vm.define_class("list", vm.List);

    vm.define_class("cons", vm.Cons, vm.List);

    vm.define_class("nil", vm.Nil, vm.List);

    vm.define_class("void", vm.Void);

    vm.define_class("ignore", vm.Ignore);

    vm.define_class("environment", vm.Environment);

    vm.define_class("class", vm.Class);

    vm.define_class("built-in-class", vm.Built_in_class, vm.Class);

    vm.define_class("standard-class", vm.Standard_class, vm.Class);

    vm.define_class("standard-object", vm.Standard_object, vm.Object, vm.Standard_class);

    vm.define_condition("condition", vm.Condition, vm.Standard_object);

    vm.define_condition("error", vm.Error, vm.Condition);

    vm.define_condition("type-error", vm.Type_error, vm.Error);

    vm.define_condition("unbound-symbol-error", vm.Unbound_symbol_error, vm.Error);

    vm.define_condition("unbound-slot-error", vm.Unbound_slot_error, vm.Error);

    vm.define_condition("unbound-method-error", vm.Unbound_method_error, vm.Error);

    vm.define_condition("assertion-error", vm.Assertion_error, vm.Error);

};

})();

var __webpack_exports__VM = __webpack_exports__.VM;
export { __webpack_exports__VM as VM };
