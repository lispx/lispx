/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
var __webpack_exports__ = {};

;// CONCATENATED MODULE: external "fs"
const external_fs_namespaceObject = require("fs");
;// CONCATENATED MODULE: external "readline"
const external_readline_namespaceObject = require("readline");
;// CONCATENATED MODULE: ./node_modules/big.js/big.mjs
/*
 *  big.js v6.2.1
 *  A small, fast, easy-to-use library for arbitrary-precision decimal arithmetic.
 *  Copyright (c) 2022 Michael Mclaughlin
 *  https://github.com/MikeMcl/big.js/LICENCE.md
 */


/************************************** EDITABLE DEFAULTS *****************************************/


  // The default values below must be integers within the stated ranges.

  /*
   * The maximum number of decimal places (DP) of the results of operations involving division:
   * div and sqrt, and pow with negative exponents.
   */
var DP = 20,          // 0 to MAX_DP

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
        if (Big.strict === true && typeof n !== 'bigint') {
          throw TypeError(INVALID + 'value');
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
    xc.length = sd;

    // Round up?
    if (more) {

      // Rounding up may mean the previous digit has to be rounded up.
      for (; ++xc[--sd] > 9;) {
        xc[sd] = 0;
        if (sd === 0) {
          ++x.e;
          xc.unshift(1);
          break;
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
 * Return a new Big whose value is the value of this Big negated.
 */
P.neg = function () {
  var x = new this.constructor(this);
  x.s = -x.s;
  return x;
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
P[Symbol.for('nodejs.util.inspect.custom')] = P.toJSON = P.toString = function () {
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


var Big = _Big_();

/// <reference types="https://raw.githubusercontent.com/DefinitelyTyped/DefinitelyTyped/master/types/big.js/index.d.ts" />
/* harmony default export */ const big_js_big = (Big);

;// CONCATENATED MODULE: ./src/eval.mjs
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
     * Evaluate a form in an environment.  This is the core internal
     * evaluation mechanism.
     *
     * The environment defaults to the VM's root environment.
     *
     * Unlike the main evaluation entry point eval_form() (in
     * control.mjs), this may return a Suspension if the code attempts
     * to capture a continuation to an outside prompt.
     */
    vm.eval = (form, env = vm.get_environment()) =>
    {
        /*
         * All exceptions that happen during evaluation, except
         * nonlocal exits and panics, are piped into the condition
         * system.
         */
        return vm.trap_exceptions(() => {
            vm.assert_type(env, vm.Environment);
            if (form instanceof vm.Symbol)
                return evaluate_symbol(form, env);
            else if (form instanceof vm.Cons)
                return evaluate_cons(form, env);
            else
                /*
                 * If the form is neither a symbol nor a cons, it
                 * evaluates to itself.
                 */
                return form;
        });
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
                       (operator) => vm.operate(operator, cons.cdr(), env),
                       vm.trace(cons.car(), env));
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
        if (operator_form instanceof vm.Symbol) {
            /*
             * A call to trap_exceptions() establishes a context from
             * which we can return with a restart.
             *
             * It's important to create such a context here, or
             * otherwise a symbol lookup error here would return from
             * the "parent" context in vm.eval().
             */
            return vm.trap_exceptions(() => {
                return env.lookup(operator_form.to_function_symbol());
            });
        } else {
            return vm.eval(operator_form, env);
        }
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
    // We could OR DEFINIEND into this, but keeping it flat produces nicer type errors.
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
            super("Match error: "
                  + vm.write_to_js_string(definiend)
                  + " vs "
                  + vm.write_to_js_string(value));
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
                           (args) => vm.operate(this.wrapped_operator, args, env),
                           vm.trace(vm.cons(this, operands), env));

            function eval_args(todo, done)
            {
                if (todo === vm.nil())
                    return vm.reverse(done);
                else
                    return vm.bind(() => vm.eval(todo.car(), env),
                                   (arg) => eval_args(todo.cdr(), vm.cons(arg, done)),
                                   vm.trace(todo.car(), env));
            }
        }

        /*
         * Returns the wrapped operator underlying the function.
         */
        unwrap()
        {
            return this.wrapped_operator;
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
        constructor(operate_function)
        {
            super();
            vm.assert_type(operate_function, "function");
            this.operate_function = operate_function;
        }

        operate(operands, env)
        {
            return this.operate_function(operands, env);
        }
    };

    /*
     * Creates a new built-in operator with the given underlying
     * JS function.
     */
    vm.built_in_operator = (fun) => new vm.Built_in_operator(fun);

    /*
     * Creates a new built-in function, that is, a wrapped built-in operator.
     */
    vm.built_in_function = (fun) => vm.wrap(vm.built_in_operator(fun));

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
    vm.alien_operator = (fun) =>
    {
        vm.assert_type(fun, "function");
        function operate_function(operands, ignored_env)
        {
            return fun.apply(null, vm.list_to_array(operands));
        }
        return vm.built_in_operator(operate_function);
    };

    /*
     * Creates a new alien function, that is, a wrapped alien operator.
     *
     * Alien functions are the main mechanism for exposing JS
     * functions to Lisp.
     */
    vm.alien_function = (fun) => vm.wrap(vm.alien_operator(fun));

    /*** The Built-In Operators ***/

    /*
     * (%vau param-tree env-param body-form) => fexpr
     *
     * Built-in operator that creates a new fexpr with the given
     * parameter tree, environment parameter, and body form.
     *
     * The dynamic environment of the call to %VAU becomes
     * the static environment of the created fexpr.
     */
    function VAU(operands, dyn_env)
    {
        const param_tree = vm.elt(operands, 0);
        const env_param = vm.elt(operands, 1);
        const body_form = vm.elt(operands, 2);
        const def_env = dyn_env;
        return new vm.Fexpr(param_tree, env_param, body_form, def_env);
    }

    /*
     * (%def definiend expression) => result
     *
     * Built-in operator that evaluates the expression and matches the
     * definiend against the result value.  Bindings are placed into
     * the dynamic environment in which %DEF is called.
     *
     * Returns the value.
     */
    function DEF(operands, env)
    {
        const definiend = vm.elt(operands, 0);
        const expression = vm.elt(operands, 1);

        return vm.bind(() => vm.eval(expression, env),
                       (result) => vm.match(definiend, result, env),
                       vm.trace(expression, env));
    }

    /*
     * (%progn . forms) => result
     *
     * Built-in operator that evaluates the forms from left to right
     * and returns the result of the last one.
     *
     * Returns #VOID if there are no forms.
     */
    function PROGN(forms, env)
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
                           },
                           vm.trace(forms.car(), env));
        }
    }

    /*
     * (%if test consequent alternative) => result
     *
     * First, evaluates the test expression which must yield a
     * boolean.
     *
     * Then, evaluates either the consequent or alternative expression
     * depending on the result of the test expression, and returns its
     * result.
     */
    function IF(operands, env)
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
                       },
                       vm.trace(test, env));
    }

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
     * trapping mechanism.  Note that panics still trigger
     * UNWIND-PROTECT and the restoration of dynamically-bound
     * variables -- otherwise Lisp invariants would get violated.
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
    vm.define_operator = (name, operator) =>
    {
        vm.get_environment().put(vm.fsym(name), operator);
    };

    /*
     * Shorthand for registering a built-in operator in the VM's root environment.
     */
    vm.define_built_in_operator = (name, fun) =>
    {
        vm.define_operator(name, vm.built_in_operator(fun));
    };

    /*
     * Shorthand for registering a built-in function in the VM's root environment.
     */
    vm.define_built_in_function = (name, js_fun) =>
    {
        vm.define_operator(name, vm.built_in_function(js_fun));
    };

    /*
     * Shorthand for registering an alien function in the VM's root environment.
     */
    vm.define_alien_function = (name, js_fun) =>
    {
        vm.define_operator(name, vm.alien_function(js_fun));
    };

    /*** Lisp API ***/

    vm.define_class("operator", vm.Operator);

    vm.define_class("built-in-operator", vm.Built_in_operator, vm.Operator);

    vm.define_class("fexpr", vm.Fexpr, vm.Operator);

    vm.define_class("function", vm.Function, vm.Operator);

    vm.define_condition("match-error", vm.Match_error, vm.Error);

    vm.define_built_in_operator("%vau", VAU);

    vm.define_built_in_operator("%def", DEF);

    vm.define_built_in_operator("%progn", PROGN);

    vm.define_built_in_operator("%if", IF);

    vm.define_alien_function("%wrap", (operator) => vm.wrap(operator));

    vm.define_alien_function("%unwrap", (fun) => vm.assert_type(fun, vm.Function).unwrap());

    vm.define_alien_function("%eval", (expr, env) => vm.eval(expr, env));

    vm.define_alien_function("%eq", (a, b) => vm.to_lisp_boolean(a === b));

    vm.define_alien_function("%=", (a, b) => vm.to_lisp_boolean(vm.equal(a, b)));

    vm.define_alien_function("%<", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) < 0));

    vm.define_alien_function("%>", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) > 0));

    vm.define_alien_function("%<=", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) <= 0));

    vm.define_alien_function("%>=", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) >= 0));

    vm.define_alien_function("%+", (a, b) => vm.add(a, b));

    vm.define_alien_function("%-", (a, b) => vm.subtract(a, b));

    vm.define_alien_function("%*", (a, b) => vm.multiply(a, b));

    vm.define_alien_function("%/", (a, b) => vm.divide(a, b));

    vm.define_alien_function("%cons", (car, cdr) => vm.cons(car, cdr));

    vm.define_alien_function("%car", (cons) => vm.assert_type(cons, vm.Cons).car());

    vm.define_alien_function("%cdr", (cons) => vm.assert_type(cons, vm.Cons).cdr());

    vm.define_alien_function("%intern", (string) => vm.intern(string));

    vm.define_alien_function("%symbol-name", (sym) =>
        vm.assert_type(sym, vm.Symbol).get_string());

    vm.define_alien_function("%variable-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_variable_symbol());

    vm.define_alien_function("%function-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_function_symbol());

    vm.define_alien_function("%class-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_class_symbol());

    vm.define_alien_function("%keyword-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_keyword_symbol());

    vm.define_alien_function("%make-environment", (parent = null) =>
        vm.make_environment(parent));

    vm.define_alien_function("%boundp", (sym, env) =>
        vm.to_lisp_boolean(vm.assert_type(env, vm.Environment).is_bound(sym)));

    vm.define_alien_function("%class-of", (obj) => vm.class_of(obj));

    vm.define_alien_function("%typep", (obj, cls) =>
        vm.to_lisp_boolean(vm.is_subclass(vm.class_of(obj), cls)));

    vm.define_alien_function("%make-instance", (cls, ...slot_inits) =>
        vm.make_instance(cls, ...slot_inits));

    vm.define_alien_function("%slot-value", (obj, slot_name) =>
        vm.assert_type(obj, vm.Standard_object).slot_value(slot_name));

    vm.define_alien_function("%set-slot-value", (obj, slot_name, slot_value) =>
        vm.assert_type(obj, vm.Standard_object).set_slot_value(slot_name, slot_value));

    vm.define_alien_function("%slot-bound-p", (obj, slot_name) =>
        vm.to_lisp_boolean(vm.assert_type(obj, vm.Standard_object).is_slot_bound(slot_name)));

    vm.define_alien_function("%add-method", (cls, name, method) =>
        vm.assert_type(cls, vm.Class).add_method(name, method));

    vm.define_alien_function("%find-method", (cls, name) =>
        vm.assert_type(cls, vm.Class).find_method(name));

    vm.define_alien_function("%make-standard-class", (name, lisp_super) =>
        vm.make_standard_class(name, lisp_super));

    vm.define_alien_function("%reinitialize-standard-class", (lisp_class, lisp_super) =>
        vm.reinitialize_standard_class(lisp_class, lisp_super));

    vm.define_alien_function("%class-name", (cls) =>
        vm.assert_type(cls, vm.Class).get_name());

    vm.define_alien_function("%subclassp", (sub_cls, super_cls) =>
        vm.to_lisp_boolean(vm.is_subclass(sub_cls, super_cls)));

    vm.define_alien_function("%panic", (exception) => vm.panic(exception));

};

;// CONCATENATED MODULE: ./src/control.mjs
/*
 * LispX Delimited Control
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds multi-prompt delimited control, delimited dynamic binding, and
 * continuation barriers to a virtual machine.
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
 * inwards from the point where continuation composition is initiated.
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
     * The innermost frame corresponds to the %TAKE-SUBCONT
     * expression that triggered continuation capture.  The outermost
     * frame corresponds to the expression that appeared directly
     * within the prompt-pushing expression (%PUSH-PROMPT or
     * %PUSH-DELIM-SUBCONT).  The prompt-pushing expression itself
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
         * frame created by the %TAKE-SUBCONT expression that triggered
         * continuation capture.
         *
         * The trace is used to display stack traces, described below.
         */
        constructor(work_fun, inner, trace)
        {
            super();
            vm.assert_type(work_fun, "function");
            vm.assert_type(inner, vm.type_or(vm.TYPE_NULL, vm.Continuation));
            this.work_fun = work_fun;
            this.inner = inner;
            this.trace = trace;
        }
    };

    /*
     * A suspension is a helper object created during the capture
     * (creation) of a continuation.
     *
     * It gets passed outwards from the %TAKE-SUBCONT expression that
     * triggers the capture until a %PUSH-PROMPT or
     * %PUSH-DELIM-SUBCONT with a matching prompt is reached.  Every
     * intervening Lisp expression adds one or more continuation
     * frames to the suspension on the way out.  Once the
     * %PUSH-PROMPT or %PUSH-DELIM-SUBCONT is reached, the
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
        suspend(work_fun, trace)
        {
            vm.assert_type(work_fun, "function");
            this.continuation = new vm.Continuation(work_fun, this.continuation, trace);
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

    /*
     * A trace may be attached to a continuation frame for debugging
     * purposes and showing the stack trace.  Traces are not needed
     * operationally, and not all frames have traces. (Every frame
     * could have a trace, but this would clutter the code.
     * Experience has shown that having traces in just a couple of
     * places (see eval.mjs) gives usable stack traces.)
     */
    class Trace
    {
        constructor(expr, env)
        {
            this.expr = expr;
            this.env = env;
        }
    }

    /*
     * Construct a trace with the given expression and environment.
     */
    vm.trace = (expr, env) => new Trace(expr, env);

    /*** Bind ***/

    /*
     * Sequences a thunk and a function in a continuation-aware
     * manner: the function receives the result of the thunk as its
     * argument.
     *
     * This is used in eval.mjs for all operators whose semantics are
     * straightforward and only require sequential execution.
     *
     * The trace is attached to the continuation frame for debugging.
     */
    vm.bind = (first, second, trace) =>
    {
        vm.assert_type(first, "function");
        vm.assert_type(second, "function");
        return do_bind(first, second, trace);
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
    function do_bind(first, second, trace, resumption = null)
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
                do_bind(first, second, trace, resumption),
                trace
            );
        else
            /*
             * The first thunk returned normally.
             * Pass its result to the second function.
             */
            return second(val);
    }

    /*** Delimited Control Operators ***/

    /*
     * (%take-subcont prompt handler) => |
     *
     * Built-in function that initiates continuation capture.  It
     * aborts outwards to the given prompt and calls the suspension
     * handler with the captured continuation.
     */
    function TAKE_SUBCONT(args, env)
    {
        const prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const handler = vm.assert_type(vm.elt(args, 1), vm.Function);

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
    }

    /*
     * (%push-prompt prompt thunk) => result
     *
     * Built-in function that pushes a prompt.  A user-supplied thunk
     * is then called inside the newly delimited continuation.
     * Returns the thunk's result.
     */
    function PUSH_PROMPT(args, env)
    {
        const prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const thunk = vm.assert_type(vm.elt(args, 1), vm.Function);
        const action = () => vm.operate(thunk, vm.nil(), env);
        return vm.push_prompt(prompt, action, env);
    }

    /*
     * (%push-delim-subcont prompt continuation thunk) => result
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
    function PUSH_DELIM_SUBCONT(args, env)
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
        return vm.push_prompt(prompt, action, env);
    }

    /*
     * Work function for PUSH_PROMPT and PUSH_DELIM_SUBCONT
     * whose difference is factored out into the action parameter.
     */
    vm.push_prompt = (prompt, action, env, resumption = null) =>
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
                    vm.push_prompt(prompt, action, env, resumption));
            }
        } else {
            /*
             * Action evaluated normally.
             */
            return result;
        }
    };

    /*
     * (%push-subcont-barrier thunk) => result
     *
     * Built-in function that calls a thunk and prevents it from
     * capturing continuations to the outside.
     */
    function PUSH_SUBCONT_BARRIER(args, env)
    {
        const thunk = vm.assert_type(vm.elt(args, 0), vm.Function);
        return vm.push_subcont_barrier(() => vm.operate(thunk, vm.nil(), env), env);
    }

    vm.push_subcont_barrier = (action, env, resumption = null) =>
    {
        /*
         * How can it be that this work function must handle
         * resumption, you ask?  Isn't the whole idea behind a
         * continuation barrier that continuations cannot escape it,
         * and therefore obviously cannot reenter it either?  The
         * answer can be found in the following comments.
         */
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = action();

        if (result instanceof vm.Suspension) {
            /*
             * Thunk attempted to capture.
             *
             * Add ourselves to the continuation.  Note that this
             * built-in is different from all others.  We do not
             * return the suspension back to the caller -- that is
             * after all exactly what we want to prevent.  But we must
             * still suspend ourselves in this way: if we didn't, the
             * barrier would be missing from the continuation after
             * re-composition.
             */
            result.suspend((resumption) =>
                vm.push_subcont_barrier(action, env, resumption));

            /*
             * Resume back into the continuation and throw an error
             * from the inside.  This means the user will be able to
             * see a useful stack trace that shows where the ill-fated
             * continuation capture occurred.
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
            super("Prompt not found: " + vm.write_to_js_string(prompt));
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
     * (%progv dynamics values thunk) => result
     *
     * Built-in function that evaluates a thunk with a list of dynamic
     * variables temporarily bound to new values taken from a second
     * list.
     *
     * Cf. Common Lisp's PROGV.
     */
    function PROGV(args, env)
    {
        const dynamics = vm.list_to_array(vm.elt(args, 0));
        const values = vm.list_to_array(vm.elt(args, 1));
        const thunk = vm.assert_type(vm.elt(args, 2), vm.Function);
        return do_progv(dynamics, values, thunk, env);
    }

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
     * This can also be used outside of the %PROGV primitive.
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
     * (%loop expr) => |
     *
     * Built-in operator that evaluates an expression in a never-ending cycle.
     *
     * Cf. Common Lisp's "simple" LOOP, not the Loop Facility.
     */
    function LOOP(operands, env)
    {
        const expr = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        return do_loop(expr, env);
    }

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
     * (%catch tag thunk) => result
     *
     * Built-in function that calls a thunk in a dynamic context where
     * a catch tag is bound.  Dynamically nested forms may nonlocally
     * exit to the catch tag with %THROW.
     *
     * Cf. Common Lisp's CATCH.
     */
    function CATCH(operands, env)
    {
        const tag = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        const thunk = vm.assert_type(vm.elt(operands, 1), vm.Function);
        return do_catch(tag, thunk, env);
    }

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
     * (%throw tag value) => |
     *
     * Built-in function that nonlocally exits to the dynamically
     * nesting catch tag and passes the value to it.
     *
     * Cf. Common Lisp's THROW.
     */
    function THROW(args, env)
    {
        const tag = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const value = vm.assert_type(vm.elt(args, 1), vm.TYPE_ANY);
        throw new vm.Nonlocal_exit(tag, value);
    }

    /*
     * Instances of this class are thrown by %THROW.
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
     * (%unwind-protect protected-expr cleanup-expr) => result
     *
     * Built-in operator that evaluates the protected expression and
     * returns its result.
     *
     * Regardless of whether the protected expression returns normally
     * or via an exception (including nonlocal exits and panics), the
     * cleanup expression is evaluated (and its result discarded)
     * after the protected expression.
     *
     * Note that the cleanup expression is not evaluated when the
     * protected expression exits via a continuation capture.
     *
     * Cf. Common Lisp's UNWIND-PROTECT.
     */
    function UNWIND_PROTECT(operands, env)
    {
        const protected_expr = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        const cleanup_expr = vm.assert_type(vm.elt(operands, 1), vm.TYPE_ANY);
        return do_unwind_protect_1(protected_expr, cleanup_expr, env);
    }

    /*
     * This must be implemented in two steps, with two work functions.
     *
     * The first one evaluates the protected expression, which may (a)
     * return normally, or (b) exit nonlocally with an exception, or (c)
     * capture a continuation.
     *
     * If it does capture, we'll have to restart at step 1 later.  If
     * it does not capture, we can go to step 2, but have to remember
     * whether step 1 returned successfully or threw an exception.
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
             * (b) Protected expression threw - go to step 2,
             * remembering that step 1 failed.
             */
            return do_unwind_protect_2(cleanup_expr, exception, false, env);
        }
    }

    /*
     * Second step of UNWIND-PROTECT.  We evaluate the cleanup
     * expression, which of course may suspend, itself.
     *
     * Afterwards we either return the result of the protected
     * expression, or rethrow the exception thrown by it.
     */
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

    /*** Root Prompt and Evaluation Entry Point ***/

    /*
     * This prompt delimits all forms evaluated by the VM.
     * It also delimits the bodies of JS lambdas (see js.lispx).
     *
     * Its purpose is to serve as a delimiter for stack traces.
     */
    const ROOT_PROMPT = vm.sym("root-prompt");

    /*
     * Evaluate a form in an environment.  This is the main entry
     * point for calling Lisp from JS.
     *
     * The environment defaults to the VM's root environment.
     *
     * Pushes a continuation barrier (to prevent continuations from
     * escaping to JS) and the root prompt (to enable taking of stack
     * traces).
     */
    vm.eval_form = (form, env = vm.get_environment()) =>
        vm.push_subcont_barrier(() =>
            vm.push_prompt(ROOT_PROMPT,
                           () => vm.eval(form, env),
                           env),
            env);

    /*** Stack Trace ***/

    /*
     * Prints the frames of a continuation as a stack trace to
     * *STANDARD-OUTPUT*.
     *
     * This should probably be implemented in Lisp but here we are.
     */
    function print_stacktrace(k)
    {
        vm.assert_type(k, vm.Continuation);
        const exprs = [];
        do {
            if (k.trace)
                exprs.push(k.trace.expr);
        } while((k = k.inner));
        exprs.reverse();
        /*
         * Drop_frames is the empirically determined amount of stack
         * inner frames to hide from a stack trace - i.e. the code of
         * signal handling and debugger itself etc.  If the boot.lispx
         * and related code changes, this needs to be updated, too.
         */
        const drop_frames = 29;
        const show_frames = 10;
        const stdout = vm.STANDARD_OUTPUT.get_value();
        exprs.slice(drop_frames, drop_frames + (show_frames - 1)).forEach((expr) => {
            stdout.fresh_line();
            vm.write(expr, stdout);
            stdout.force_output();
        });
    }

    /*** Lisp API ***/

    vm.define_class("continuation", vm.Continuation);

    vm.define_class("dynamic", vm.Dynamic, vm.Standard_object, vm.Standard_class);

    vm.define_condition("prompt-not-found-error", vm.Prompt_not_found_error, vm.Error);

    vm.define_built_in_function("%take-subcont", TAKE_SUBCONT);

    vm.define_built_in_function("%push-prompt", PUSH_PROMPT);

    vm.define_built_in_function("%push-delim-subcont", PUSH_DELIM_SUBCONT);

    vm.define_built_in_function("%push-subcont-barrier", PUSH_SUBCONT_BARRIER);

    vm.define_built_in_function("%progv", PROGV);

    vm.define_built_in_operator("%loop", LOOP);

    vm.define_built_in_function("%catch", CATCH);

    vm.define_built_in_function("%throw", THROW);

    vm.define_built_in_operator("%unwind-protect", UNWIND_PROTECT);

    vm.define_constant("+root-prompt+", ROOT_PROMPT);

    vm.define_alien_function("%print-stacktrace", print_stacktrace);

};

;// CONCATENATED MODULE: ./src/seq.mjs
/*
 * LispX Sequence and List Processing Utilities
 * Copyright (c) 2021, 2022 Manuel J. Simoni
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
     * Creates a new list by calling a function on every element of a list.
     */
    vm.mapcar = (fun, list) =>
    {
        if (list === vm.nil())
            return vm.nil();
        else
            return vm.cons(fun(list.car()), vm.mapcar(fun, list.cdr()));
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

    vm.define_alien_function("%list*", (...objects) => vm.list_star(...objects));

    vm.define_alien_function("%append", (list1, list2) => vm.append(list1, list2));

    vm.define_alien_function("%list-length", (list) => vm.num(vm.list_length(list)));

    vm.define_alien_function("%nth", (num, list) =>
        vm.elt(list, vm.assert_type(num, vm.Number).to_js_number()));

    vm.define_alien_function("%nthcdr", (num, list) =>
        vm.nthcdr(vm.assert_type(num, vm.Number).to_js_number(), list));

    vm.define_alien_function("%list-subseq", (list, start, end) =>
        vm.list_subseq(list,
                       vm.assert_type(start, vm.Number).to_js_number(),
                       canonicalize_end(end)));

    vm.define_alien_function("%string-subseq", (string, start, end) =>
        vm.string_subseq(string,
                         vm.assert_type(start, vm.Number).to_js_number(),
                         canonicalize_end(end)));

    vm.define_alien_function("%reverse", (list) => vm.reverse(list));

    vm.define_condition("out-of-bounds-error", vm.Out_of_bounds_error, vm.Error);

};

;// CONCATENATED MODULE: ./src/stream.mjs
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

    vm.define_alien_function("%make-string-input-stream", (string) =>
        new vm.String_input_stream(string));

    vm.define_alien_function("%make-string-output-stream", () =>
        new vm.String_output_stream());

    vm.define_alien_function("%get-output-stream-string", (sos) =>
        vm.assert_type(sos, vm.String_output_stream).get_string());

    vm.define_alien_function("%fresh-line", (stream) =>
        vm.assert_type(stream, vm.Output_stream).fresh_line());

    vm.define_alien_function("%force-output", (stream) =>
        vm.assert_type(stream, vm.Output_stream).force_output());

};

;// CONCATENATED MODULE: ./src/read.mjs
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

        /*
         * Why do we need a unique EOF object can't just check that
         * the read object isn't the dot?  Because the user might have
         * passed in the dot symbol as their eof_value.
         */
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
            case "^":
                return read_namespaced_symbol(stream, vm.CLASS_NAMESPACE);
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

    vm.define_alien_function("%read", (stream, eof_error_p, eof_value) => {
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

;// CONCATENATED MODULE: ./src/print.mjs
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
     * (Option.)
     *
     * If nil, do not abbreviate.
     */
    vm.PRINT_LEVEL_OPTION = vm.make_dynamic(vm.nil());

    /*
     * The current level we are printing at, to be compared against
     * *PRINT-LEVEL?*.  Increased every time WRITE is entered, so
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
     * Print a non-Lisp JS object.
     */
    vm.write_js_object = (object, stream) =>
    {
        stream.write_string(vm.str("#<<js "
                                   + typeof(object)
                                   + " "
                                   + String(object)
                                   + ">>"));
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
        stream.write_string(vm.str("()"));
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
        case vm.CLASS_NAMESPACE: return vm.str("#^");
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
     * Call the thunk if *PRINT-LEVEL?* is nil, or if
     * *CURRENT-PRINT-LEVEL* is less than the contents of
     * *PRINT-LEVEL?*.
     *
     * Otherwise, print "#" to the stream.
     */
    function maybe_abbreviate_object_based_on_current_print_level(stream, thunk)
    {
        const print_level_option = vm.PRINT_LEVEL_OPTION.get_value();
        const current_print_level = vm.CURRENT_PRINT_LEVEL.get_value();
        if ((print_level_option === vm.nil())
            || (vm.compare(current_print_level, print_level_option.car()) < 0)) {
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

    vm.define_variable("*print-level?*", vm.PRINT_LEVEL_OPTION);

    vm.define_alien_function("%write", (object, stream) => vm.write(object, stream));

};

;// CONCATENATED MODULE: ./src/js.mjs
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
     * Transforms a JS boolean into a Lisp one.
     */
    vm.to_lisp_boolean = (js_bool) =>
    {
        return vm.assert_type(js_bool, "boolean") ? vm.t() : vm.f();
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

    /*
     * Output stream that prints to the JS console.
     *
     * The JS console does not allow appending to the current line --
     * it only supports outputting a full line by itself.  This means
     * that sometimes, such as when the user does a FORCE-OUTPUT and
     * there is some buffered data, we will have to output a full
     * line, even though there might be no newline in the actual data
     * written by the user.
     *
     * The code currently does not specifically handle CR or LF
     * output, so printing those might lead to weird results.
     *
     * The code currently does not force the output by itself, so
     * FORCE-OUTPUT (or a function that calls it, like PRINT) must be
     * called from time to time.
     */
    vm.JS_console_output_stream = class Lisp_js_console_output_stream extends vm.Output_stream
    {
        /*
         * The output_function can be overridden for testing and
         * printing to other line-based systems, such as the REPL
         * terminal.
         */
        constructor(output_function = console.log)
        {
            super();
            this.buffer = "";
            this.output_function = output_function;
        }

        /*
         * See stream.mjs for the documentation of the output stream API
         * methods.
         */

        write_byte(b)
        {
            vm.assert_type(b, "string");
            vm.assert(b.length === 1);
            this.buffer += b;
            return b;
        }

        fresh_line()
        {
            /*
             * If the buffer is empty, or the last byte is a newline,
             * we don't need to do anything.
             */
            if ((this.buffer.length === 0)
                || (this.buffer[this.buffer.length - 1] === "\n")) {
                return vm.f();
            } else {
                this.write_byte("\n");
                return vm.t();
            }
        }

        force_output()
        {
            if (this.buffer.length > 0) {
                this.output_function(vm.utf8_decode(this.buffer));
                this.buffer = "";
            }
            return vm.void();
        }
    };

    /*** Lisp API ***/

    vm.define_constant("+js-true+", true);

    vm.define_constant("+js-false+", false);

    vm.define_constant("+js-null+", null);

    vm.define_constant("+js-undefined+", undefined);

    vm.define_alien_function("%js-global", vm.js_global);

    vm.define_alien_function("%js-new", vm.js_new);

    vm.define_alien_function("%js-get", vm.js_get);

    vm.define_alien_function("%js-elt", vm.js_elt);

    vm.define_alien_function("%to-lisp-boolean", vm.to_lisp_boolean);

    vm.define_alien_function("%to-js-boolean", (bool) =>
        vm.assert_type(bool, vm.Boolean).to_js_boolean());

    vm.define_alien_function("%to-lisp-number", (js_num) =>
        vm.num(vm.assert_type(js_num, "number")));

    vm.define_alien_function("%to-js-number", (num) =>
        vm.assert_type(num, vm.Number).to_js_number());

    vm.define_alien_function("%to-lisp-string", (js_str) =>
        vm.str(vm.assert_type(js_str, "string")));

    vm.define_alien_function("%to-js-string", (str) =>
        vm.assert_type(str, vm.String).to_js_string());

    vm.define_alien_function("%to-lisp-function", vm.to_lisp_function);

    vm.define_alien_function("%to-js-function", vm.to_js_function);

    vm.define_alien_function("%list-to-js-array", vm.list_to_array);

    vm.define_alien_function("%js-array-to-list", vm.array_to_list);

    vm.define_alien_function("%apply-js-method", vm.apply_js_method);

    vm.define_alien_function("%js-log", (...objects) => console.log(...objects));

    vm.define_alien_function("%sleep", (ms) => {
        vm.assert_type(ms, vm.Number);
        return new Promise(resolve => setTimeout(resolve, ms.to_js_number()));
    });

    vm.define_class("js-console-output-stream", vm.JS_console_output_stream, vm.Output_stream);

    /*
     * Register a JS console output stream as standard output.
     */
    vm.STANDARD_OUTPUT.set_value(new vm.JS_console_output_stream());

};

;// CONCATENATED MODULE: ./src/boot.lispx
/* harmony default export */ const boot_lispx = ("(%def #'list (%wrap (%vau arguments #ignore arguments))) (%def #'vau (%vau (parameter-tree environment-parameter . forms) env (%eval (list #'%vau parameter-tree environment-parameter (%list* #'%progn forms)) env))) (%def #'lispx::make-macro (%wrap (%vau (expander) #ignore (%vau operand env (%eval (%eval (%cons expander operand) (%make-environment)) env))))) (%def #'macro (lispx::make-macro (%vau (parameter-tree . forms) #ignore (list #'lispx::make-macro (%list* #'vau parameter-tree #ignore forms))))) (%def #'defmacro (macro (name parameter-tree . forms) (list #'%def (%function-symbol name) (%list* #'macro parameter-tree forms)))) (defmacro defexpr (name parameter-tree environment-parameter . forms) (list #'%def (%function-symbol name) (%list* #'vau parameter-tree environment-parameter forms))) (defmacro def (definiend-tree value . docstring?) (list #'%def definiend-tree value)) (defmacro defconstant (name value . docstring?) (list #'def name value)) (defmacro lambda (parameter-tree . forms) (list #'%wrap (%list* #'vau parameter-tree #ignore forms))) (defmacro defun (name parameter-tree . forms) (list #'def (%function-symbol name) (%list* #'lambda parameter-tree forms))) (defmacro progn forms (list* #'%progn forms)) (defmacro if (test consequent alternative) (list #'%if test consequent alternative)) (defmacro catch (tag . forms) (list #'%catch tag (list* #'lambda () forms))) (defun throw (tag . result?) (%throw tag (optional result?))) (defmacro loop forms (list #'%loop (list* #'progn forms))) (defun eq (a b) (%eq a b)) (defun class-of (object) (%class-of object)) (defun typep (object class) (%typep object class)) (defun intern (string) (%intern string)) (defun symbol-name (symbol) (%symbol-name symbol)) (defun variable-symbol (symbol) (%variable-symbol symbol)) (defun function-symbol (symbol) (%function-symbol symbol)) (defun class-symbol (symbol) (%class-symbol symbol)) (defun keyword-symbol (symbol) (%keyword-symbol symbol)) (defun cons (car cdr) (%cons car cdr)) (defun car (cons) (%car cons)) (defun cdr (cons) (%cdr cons)) (defun list* arguments (apply #'%list* arguments)) (defun reverse (list) (%reverse list)) (defun wrap (operator) (%wrap operator)) (defun unwrap (function) (%unwrap function)) (defun eval (form environment) (%eval form environment)) (defun make-environment parent-environment? (apply #'%make-environment parent-environment?)) (defun boundp (symbol environment) (%boundp symbol environment)) (defun panic (error) (%panic error)) (defun invoke-debugger (condition) (take-subcont +root-prompt+ k (push-delim-subcont +root-prompt+ k (%print-stacktrace k) (panic condition)))) (defmacro let (bindings . forms) (list* (list* #'lambda (mapcar #'car bindings) forms) (mapcar #'cadr bindings))) (defmacro let* (bindings . forms) (if (null bindings) (list* #'let () forms) (list #'let (list (car bindings)) (list* #'let* (cdr bindings) forms)))) (defmacro lispx::letrec (bindings . forms) (if (null bindings) (list* #'let () forms) (list* #'let () (list #'def (mapcar #'car bindings) (list* #'list (mapcar #'cadr bindings))) forms))) (defun lispx::make-function-binding ((name parameter-tree . forms)) (list (function-symbol name) (list* #'lambda parameter-tree forms))) (defmacro flet (function-bindings . forms) (list* #'let (mapcar #'lispx::make-function-binding function-bindings) forms)) (defmacro labels (function-bindings . forms) (list* #'lispx::letrec (mapcar #'lispx::make-function-binding function-bindings) forms)) (defexpr quote (operand) #ignore operand) (defexpr the-environment () environment environment) (defun apply (function arguments) (eval (cons (unwrap function) arguments) (%make-environment))) (defmacro when (test . forms) (list #'if test (list* #'progn forms) #void)) (defmacro unless (test . forms) (list #'if test #void (list* #'progn forms))) (defexpr cond clauses env (unless (null clauses) (let ((((test . forms) . rest-clauses) clauses)) (if (eval test env) (eval (cons #'progn forms) env) (eval (cons #'cond rest-clauses) env))))) (defun not (boolean) (if boolean #f #t)) (defexpr and operands env (cond ((null operands) #t) ((null (cdr operands)) (the boolean (eval (car operands) env))) ((eval (car operands) env) (eval (cons #'and (cdr operands)) env)) (#t #f))) (defexpr or operands env (cond ((null operands) #f) ((null (cdr operands)) (the boolean (eval (car operands) env))) ((eval (car operands) env) #t) (#t (eval (cons #'or (cdr operands)) env)))) (defexpr while (test-form . forms) env (let ((forms (list* #'progn forms))) (block exit (loop (if (eval test-form env) (eval forms env) (return-from exit)))))) (defmacro until (test-form . forms) (list* #'while (list #'not test-form) forms)) (defmacro dotimes ((var count-form . result-form?) . body-forms) (flet ((_dotimes_ (n #'body #'result) (let ((#'i (box 0))) (while (< (i) n) (body (i)) (i (+ (i) 1))) (result (i))))) (list #'_dotimes_ count-form (list* #'lambda (list var) body-forms) (list* #'lambda (list var) result-form?)))) (defmacro loop-let (name initializers . forms) (list #'labels (list (list* name (mapcar #'car initializers) forms)) (list* name (mapcar #'cadr initializers)))) (defexpr block (block-name . forms) env (let ((tag (list #void))) (flet ((escape (value) (throw tag value))) (catch tag (eval (list (list* #'lambda (list block-name) forms) #'escape) env))))) (defun return-from (#'block-name . value?) (block-name (optional value?))) (defmacro unwind-protect (protected-form . cleanup-forms) (list #'%unwind-protect protected-form (list* #'progn cleanup-forms))) (defexpr prog1 (form . forms) env (let ((result (eval form env))) (eval (list* #'progn forms) env) result)) (defun lispx::make-typecase-with-default-function (#'default) (vau (keyform . clauses) env (let ((key (eval keyform env))) (loop-let -typecase- ((clauses clauses)) (if (null clauses) (default key) (let ((((class-name . forms) . rest-clauses) clauses)) (if (typep key (find-class class-name env)) (eval (list* #'progn forms) env) (-typecase- rest-clauses)))))))) (def #'typecase (lispx::make-typecase-with-default-function (lambda (#ignore) #void))) (def #'etypecase (lispx::make-typecase-with-default-function (lambda (key) (error (make-type-error key #^object))))) (defexpr set (environment definiend-tree value) dynamic-environment (eval (list #'def definiend-tree (list (unwrap #'eval) value dynamic-environment)) (eval environment dynamic-environment))) (defun box initial-value? (def value (optional initial-value?)) (def env (the-environment)) (lambda new-value? (if-option (new-value new-value?) (set env value new-value) value))) (defun assert (boolean) (unless boolean (error (make-instance #^assertion-error)))) (defun compose (#'f #'g) (lambda args (g (apply #'f args)))) (defun identity (x) x) (defun null (object) (eq object ())) (defun consp (object) (typep object #^cons)) (defun caar (cons) (car (car cons))) (defun cadr (cons) (car (cdr cons))) (defun cdar (cons) (cdr (car cons))) (defun cddr (cons) (cdr (cdr cons))) (defun append (list1 list2) (%append list1 list2)) (defun nth (n list) (%nth n list)) (defun nthcdr (n list) (%nthcdr n list)) (defun mapcar (#'function list) (if (null list) () (cons (function (car list)) (mapcar #'function (cdr list))))) (defun mapc (#'function list) (unless (null list) (function (car list)) (mapc #'function (cdr list))) list) (defun mapcan (#'function list) (if (null list) () (append (function (car list)) (mapcan #'function (cdr list))))) (defmacro dolist ((var list-form . result-form?) . body-forms) (labels ((_dolist_ (list #'body #'result) (if (null list) (result list) (progn (body (car list)) (_dolist_ (cdr list) #'body #'result))))) (list #'_dolist_ list-form (list* #'lambda (list var) body-forms) (list* #'lambda (list var) result-form?)))) (defun reduce (#'function list :initial-value initial-value) (if (null list) initial-value (reduce #'function (cdr list) :initial-value (function initial-value (car list))))) (defun member (item list . keywords) (let ((#'test (optional (get? keywords :test) #'eq)) (#'key (optional (get? keywords :key) #'identity))) (loop-let -member- ((items list)) (if (null items) () (if (test item (key (car items))) items (-member- (cdr items))))))) (defun remove-if (#'test list) (if (null list) () (if (test (car list)) (remove-if #'test (cdr list)) (cons (car list) (remove-if #'test (cdr list)))))) (defun get? (plist indicator) (if (null plist) () (let (((i v . plist) plist)) (if (eq i indicator) (some v) (get? plist indicator))))) (defun lispx::make-relational-operator (#'binary-operator) (labels ((operator (arg1 arg2 . rest) (if (binary-operator arg1 arg2) (if (null rest) #t (apply #'operator (list* arg2 rest))) #f))) #'operator)) (def #'= (lispx::make-relational-operator #'%=)) (def #'< (lispx::make-relational-operator #'%<)) (def #'> (lispx::make-relational-operator #'%>)) (def #'<= (lispx::make-relational-operator #'%<=)) (def #'>= (lispx::make-relational-operator #'%>=)) (defun /= (arg . args) (if (null args) #t (if (consp (member arg args :test #'=)) #f (apply #'/= args)))) (defun lispx::make-thetic-operator (#'binary-operator initial-value) (lambda args (reduce #'binary-operator args :initial-value initial-value))) (def #'+ (lispx::make-thetic-operator #'%+ 0)) (def #'* (lispx::make-thetic-operator #'%* 1)) (defun lispx::make-lytic-operator (#'binary-operator initial-value) (lambda (arg1 . rest) (if (null rest) (binary-operator initial-value arg1) (reduce #'binary-operator rest :initial-value arg1)))) (def #'- (lispx::make-lytic-operator #'%- 0)) (def #'/ (lispx::make-lytic-operator #'%/ 1)) (defun find-class (name environment) (eval (class-symbol name) environment)) (defun class-name (class) (%class-name class)) (defun subclassp (class superclass) (%subclassp class superclass)) (defexpr defclass (name superclass? slot-specs . properties) env (dolist (slot-spec slot-specs) (the symbol slot-spec)) (let ((class-name (class-symbol name)) (superclass (find-class (optional superclass? (quote standard-object)) env))) (if (boundp class-name env) (%reinitialize-standard-class (eval class-name env) superclass) (eval (list #'def class-name (%make-standard-class name superclass)) env)))) (defexpr defgeneric (name (receiver . parameters) . properties) env (flet ((generic args (apply (%find-method (class-of (car args)) name) args))) (eval (list #'def (function-symbol name) #'generic) env))) (defexpr defmethod (name ((receiver class-name) . parameters) . forms) env (let ((#'method (eval (list* #'lambda (list* receiver parameters) forms) env))) (%add-method (find-class class-name env) name #'method))) (defun make-instance (class . slot-inits) (apply #'%make-instance (cons class slot-inits))) (defun slot-value (object slot-name) (%slot-value object slot-name)) (defun set-slot-value (object slot-name value) (%set-slot-value object slot-name value)) (defun slot-bound-p (object slot-name) (%slot-bound-p object slot-name)) (defun make-type-error (datum expected-type) (make-instance #^type-error :datum datum :expected-type expected-type)) (defun assert-type (object class) (if (typep object class) object (error (make-type-error object (class-name class))))) (defexpr the (class-name object) env (assert-type (eval object env) (find-class class-name env))) (defgeneric length (sequence)) (defmethod length ((seq list)) (%list-length seq)) (defgeneric elt (sequence index)) (defmethod elt ((seq list) index) (nth index seq)) (defgeneric subseq (sequence start . end?)) (defmethod subseq ((seq list) start . end?) (%list-subseq seq start (optional end?))) (defmethod subseq ((seq string) start . end?) (%string-subseq seq start (optional end?))) (defun some (value) (list value)) (defexpr if-option ((name option?) then else) env (let ((o? (eval option? env))) (if (null o?) (eval else env) (eval (list (list #'vau (list name) #ignore then) (car o?)) env)))) (defmacro when-option ((name option?) . forms) (list #'if-option (list name option?) (list* #'progn forms) ())) (defmacro unless-option (option? . forms) (list #'if-option (list #ignore option?) () (list* #'progn forms))) (defexpr optional (option? . default?) env (if-option (value (eval option? env)) value (if-option (default default?) (eval default env) #void))) (defexpr optionals (list . defaults) env (loop-let -optionals- ((list (eval list env)) (defaults defaults)) (if (null list) (if (null defaults) () (cons (eval (car defaults) env) (-optionals- () (cdr defaults)))) (if (null defaults) (cons (car list) (-optionals- (cdr list) ())) (cons (car list) (-optionals- (cdr list) (cdr defaults))))))) (defun get-option (option?) (optional option? (simple-error \"Option is nil\"))) (defexpr defdynamic (name . value-and-docstring?) env (def value (eval (optional value-and-docstring?) env)) (if (boundp name env) (set-dynamic (eval name env) value) (eval (list #'def name (make-instance #^dynamic :value value)) env))) (defun dynamic (dynamic-variable) (slot-value dynamic-variable (quote value))) (defun set-dynamic (dynamic-variable value) (set-slot-value dynamic-variable (quote value) value)) (defexpr dynamic-let (bindings . forms) env (let ((dynamics (mapcar (lambda ((name #ignore)) (eval name env)) bindings)) (values (mapcar (lambda ((#ignore value)) (eval value env)) bindings)) (thunk (eval (list* #'lambda () forms) env))) (%progv dynamics values thunk))) (defmacro dynamic-let* (bindings . forms) (if (null bindings) (list* #'progn forms) (list #'dynamic-let (list (car bindings)) (list* #'dynamic-let* (cdr bindings) forms)))) (defmacro progv (dynamic-variables values . forms) (list #'%progv dynamic-variables values (list* #'lambda () forms))) (defmacro push-prompt (prompt . forms) (list #'%push-prompt prompt (list* #'lambda () forms))) (defmacro take-subcont (prompt name . forms) (list #'%take-subcont prompt (list* #'lambda (list name) forms))) (defmacro push-delim-subcont (prompt continuation . forms) (list #'%push-delim-subcont prompt continuation (list* #'lambda () forms))) (defmacro push-subcont-barrier forms (list #'%push-subcont-barrier (list* #'lambda () forms))) (defconstant +default-prompt+ (quote default-prompt) \"This prompt is used for general coroutine-like use of\ncontinuations.\") (defmacro coroutine forms (list* #'push-prompt (quote +default-prompt+) forms)) (defmacro yield (name . forms) (list* #'take-subcont (quote +default-prompt+) name forms)) (defmacro resume (k . forms) (list* #'push-delim-subcont (quote +default-prompt+) k forms))");
;// CONCATENATED MODULE: ./src/cond-sys.lispx
/* harmony default export */ const cond_sys_lispx = ("(defclass handler-frame () (handlers parent-frame?)) (defclass condition-handler () (condition-class handler-function)) (defclass restart-handler () (restart-name handler-function interactive-function? associated-conditions)) (defdynamic *condition-handler-frame?* ()) (defdynamic *restart-handler-frame?* ()) (defun lispx::make-handler-bind-operator (#'handler-spec-parser handler-frame-dynamic) (vau (handler-specs . forms) env (let ((handler-frame (make-instance #^handler-frame :handlers (mapcar (lambda (spec) (handler-spec-parser spec env)) handler-specs) :parent-frame? (dynamic handler-frame-dynamic)))) (progv (list handler-frame-dynamic) (list (some handler-frame)) (eval (list* #'progn forms) env))))) (def #'handler-bind (lispx::make-handler-bind-operator (lambda ((class-name function-form) env) (make-instance #^condition-handler :condition-class (the class (find-class class-name env)) :handler-function (the function (eval function-form env)))) *condition-handler-frame?*)) (def #'restart-bind (lispx::make-handler-bind-operator (lambda ((restart-name function-form . properties) env) (make-instance #^restart-handler :restart-name (the symbol restart-name) :handler-function (the function (eval function-form env)) :interactive-function? (when-option (i-f-form (get? properties :interactive-function)) (some (the function (eval i-f-form env)))) :associated-conditions (when-option (a-cs-form (get? properties :associated-conditions)) (the list (eval a-cs-form env))))) *restart-handler-frame?*)) (defun lispx::make-handler-case-operator (#'handler-bind-operator) (vau (handler-specs . forms) env (block exit ((block trampoline (eval (list #'handler-bind-operator (mapcar (lambda ((name function-form . properties)) (list* name (lambda args (return-from trampoline (lambda () (apply (eval function-form env) args)))) properties)) handler-specs) (list #'return-from exit (list* #'progn forms))) env)))))) (def #'handler-case (lispx::make-handler-case-operator #'handler-bind)) (def #'restart-case (lispx::make-handler-case-operator #'restart-bind)) (defun _signal_ (condition) (loop-let -signal- ((handler-frame? (dynamic *condition-handler-frame?*))) (if-option ((handler frame) (lispx::find-handler? condition handler-frame? ())) (progn (lispx::call-condition-handler handler frame condition) (-signal- (slot-value frame (quote parent-frame?)))) #void))) (defun lispx::call-condition-handler (handler handler-frame condition) (dynamic-let ((*condition-handler-frame?* (slot-value handler-frame (quote parent-frame?)))) (lispx::apply-handler-function handler (list condition)))) (defun lispx::apply-handler-function (handler arguments) (apply (slot-value handler (quote handler-function)) arguments)) (defun _error_ (condition) (signal condition) (invoke-debugger condition)) (defun lispx::make-signal-with-restarts-operator (#'signal-operator) (vau (condition . handler-specs) env (let ((c (eval condition env))) (flet ((append-associated-condition (handler-spec) (append handler-spec (list :associated-conditions (list #'list c))))) (eval (list #'restart-case (mapcar #'append-associated-condition handler-specs) (list #'signal-operator c)) env))))) (def #'signal (lispx::make-signal-with-restarts-operator #'_signal_)) (def #'error (lispx::make-signal-with-restarts-operator #'_error_)) (defun invoke-restart (restart-designator . arguments) (lispx::invoke-restart-with-arguments-producing-function restart-designator (lambda (#ignore) arguments))) (defun invoke-restart-interactively (restart-designator) (lispx::invoke-restart-with-arguments-producing-function restart-designator (lambda (restart-handler) (when-option (#'i-f (slot-value restart-handler (quote interactive-function?))) (i-f))))) (defun lispx::invoke-restart-with-arguments-producing-function (restart-designator #'function) (etypecase restart-designator (symbol (if-option (restart-handler (find-restart? restart-designator)) (lispx::apply-handler-function restart-handler (function restart-handler)) (error (make-restart-error restart-designator)))) (restart-handler (lispx::apply-handler-function restart-designator (function restart-designator))))) (defun lispx::find-handler? (object handler-frame? payload?) (when-option (handler-frame handler-frame?) (block found (dolist (handler (slot-value handler-frame (quote handlers))) (when (lispx::handler-applicable-p handler object payload?) (return-from found (some (list handler handler-frame))))) (lispx::find-handler? object (slot-value handler-frame (quote parent-frame?)) payload?)))) (defun find-restart? (name . condition?) (when-option ((handler #ignore) (lispx::find-handler? name (dynamic *restart-handler-frame?*) condition?)) (some handler))) (defgeneric lispx::handler-applicable-p (handler object payload?)) (defmethod lispx::handler-applicable-p ((handler condition-handler) condition ()) (typep condition (slot-value handler (quote condition-class)))) (defmethod lispx::handler-applicable-p ((handler restart-handler) restart-name condition?) (and (eq restart-name (slot-value handler (quote restart-name))) (lispx::restart-handler-applicable-to-condition-p handler condition?))) (defun lispx::restart-handler-applicable-to-condition-p (handler condition?) (if-option (condition condition?) (let ((a-cs (slot-value handler (quote associated-conditions)))) (if (null a-cs) #t (consp (member condition a-cs)))) #t)) (defun compute-restarts condition? (loop-let -compute-restarts- ((restarts (quote ())) (handler-frame? (dynamic *restart-handler-frame?*))) (if-option (handler-frame handler-frame?) (-compute-restarts- (append restarts (remove-if (lambda (restart) (not (lispx::restart-handler-applicable-to-condition-p restart condition?))) (slot-value handler-frame (quote handlers)))) (slot-value handler-frame (quote parent-frame?))) restarts))) (defclass restart-error (error) (restart-name)) (defun make-restart-error (restart-name) (make-instance #^restart-error :restart-name restart-name)) (defclass simple-error (error) (message)) (defun make-simple-error (message) (make-instance #^simple-error :message message)) (defun simple-error (message) (error (make-simple-error message)))");
;// CONCATENATED MODULE: ./src/stream.lispx
/* harmony default export */ const stream_lispx = ("(defun make-string-input-stream (string) (%make-string-input-stream string)) (defexpr with-standard-input-from-string (string . forms) env (let ((s (eval string env))) (dynamic-let ((*standard-input* (make-string-input-stream s))) (eval (list* #'progn forms) env)))) (defun make-string-output-stream () (%make-string-output-stream)) (defun get-output-stream-string (stream) (%get-output-stream-string stream)) (defexpr with-standard-output-to-string forms env (dynamic-let ((*standard-output* (make-string-output-stream))) (eval (list* #'progn forms) env) (get-output-stream-string (dynamic *standard-output*)))) (defun fresh-line stream? (%fresh-line (optional stream? (dynamic *standard-output*)))) (defun force-output stream? (%force-output (optional stream? (dynamic *standard-output*))))");
;// CONCATENATED MODULE: ./src/read.lispx
/* harmony default export */ const read_lispx = ("(defun read arguments (apply #'stream-read (optionals arguments (dynamic *standard-input*) #t #void))) (defgeneric stream-read (stream eof-error-p eof-value)) (defmethod stream-read ((stream input-stream) eof-error-p eof-value) (%read stream eof-error-p eof-value))");
;// CONCATENATED MODULE: ./src/print.lispx
/* harmony default export */ const print_lispx = ("(defun write (object . keywords) (%write object (optional (get? keywords :stream) (dynamic *standard-output*)))) (defun write-to-string (object) (with-standard-output-to-string (write object))) (defun print1 (object) (dynamic-let ((*print-escape* #t)) (write object))) (defun uprint1 (object) (dynamic-let ((*print-escape* #f)) (write object))) (defun print (object) (fresh-line) (prog1 (print1 object) (force-output))) (defun uprint (object) (fresh-line) (prog1 (uprint1 object) (force-output)))");
;// CONCATENATED MODULE: ./src/js.lispx
/* harmony default export */ const js_lispx = ("(defun js-eq (a b) (eq a b)) (defun apply-js-function (js-function arguments) (apply (to-lisp-function js-function) arguments)) (defun call-js-function (js-function . arguments) (apply-js-function js-function arguments)) (defmacro js-lambda (parameter-tree . forms) (list #'to-js-function (list #'lambda parameter-tree (list #'push-subcont-barrier (list* #'push-prompt (quote +root-prompt+) forms))))) (defun js-global (name) (%js-global name)) (defun js-new (constructor . arguments) (apply #'%js-new (cons constructor arguments))) (defun js-get (object name) (%js-get object name)) (defun to-lisp-boolean (js-boolean) (%to-lisp-boolean js-boolean)) (defun to-js-boolean (lisp-boolean) (%to-js-boolean lisp-boolean)) (defun to-lisp-number (js-number) (%to-lisp-number js-number)) (defun to-js-number (lisp-number) (%to-js-number lisp-number)) (defun to-lisp-string (js-string) (%to-lisp-string js-string)) (defun to-js-string (lisp-string) (%to-js-string lisp-string)) (defun to-lisp-function (js-function) (%to-lisp-function js-function)) (defun to-js-function (lisp-operator) (%to-js-function lisp-operator)) (defun list-to-js-array (list) (%list-to-js-array list)) (defun js-array-to-list (array) (%js-array-to-list array)) (defun js-array elements (list-to-js-array elements)) (defmethod elt ((seq object) index) (if (or (< index 0) (>= index (length seq))) (error (make-instance #^out-of-bounds-error)) (%js-elt seq index))) (defmethod length ((seq object)) (to-lisp-number (js-get seq \"length\"))) (defun apply-js-method (receiver name arguments) (%apply-js-method receiver name arguments)) (defun call-js-method (receiver name . arguments) (apply #'apply-js-method (list receiver name arguments))) (defun js-method (method-name) (lambda (receiver . arguments) (apply-js-method receiver method-name arguments))) (defmacro define-js-method (name method-name) (list #'def (function-symbol name) (js-method method-name))) (defun js-undefined-option (value) (if (eq value +js-undefined+) () (some value))) (defun js-null-option (value) (if (eq value +js-null+) () (some value))) (defun await (promise) (yield k (call-js-method promise \"then\" (js-lambda (value) (resume k value)) (js-lambda (error) (resume k (error error)))))) (defun sync (#'fun) (lambda args (await (apply #'fun args)))) (defmacro define-js-method/sync (name method-name) (list #'def (function-symbol name) (sync (js-method method-name)))) (defun sleep (ms) (await (%sleep ms))) (defun js-log arguments (apply #'%js-log arguments))");
;// CONCATENATED MODULE: ./src/vm.mjs
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
        init_eval(this);
        init_control(this);
        init_seq(this);
        init_stream(this);
        init_read(this);
        init_print(this);
        init_js(this);

        /*
         * Evaluate the bootstrap code.
         */
        this.eval_js_string(boot_lispx);
        this.eval_js_string(cond_sys_lispx);
        this.eval_js_string(stream_lispx);
        this.eval_js_string(read_lispx);
        this.eval_js_string(print_lispx);
        this.eval_js_string(js_lispx);
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
        return new this.Number(new big_js_big(js_string_or_number));
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
     * Lisp objects are created by normal JavaScript constructor
     * functions.  The JS class that each object is an instance of
     * points to the corresponding class metaobject with
     * prototype.lisp_class (see section on Classes below).  This
     * means that every Lisp object also inherits this lisp_class
     * property pointing to its Lisp class.
     *
     * JavaScript objects that do not have a lisp_class property are
     * treated as being instances of the root class of the Lisp class
     * hierarchy, OBJECT.  There is no further distinction between JS
     * strings, numbers, etc when viewed from Lisp, they are all
     * instances of OBJECT.
     *
     * Slots of standard objects are stored as JS properties prefixed
     * with "lisp_slot_".  For example, instances of a class defined
     * as (defclass point () (x y)) will have slots lisp_slot_x and
     * lisp_slot_y.
     *
     * Methods are stored as JS properties of JS classes prefixed with
     * "lisp_method_".
     */

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
     *
     * Lisp strings are UTF-8 strings, while JavaScript uses UTF-16
     * natively.
     *
     * Lisp strings are stored as UTF-16 strings in a somewhat unusual
     * encoding: every UTF-8 8-bit byte is stored as one UTF-16 16-bit
     * code unit (not to be confused with Unicode code points).
     *
     * So the UTF-16 string for the Euro symbol, `"\u{20AC}"`, becomes
     * `"\u{E2}\u{82}\u{AC}"` as a Lisp string.  The UTF-16 string has
     * one code unit; the UTF-8 string has three code units, each
     * corresponding to one UTF-8 byte.
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
            vm.assert_type(big, big_js_big);
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
     * Every Lisp class is represented as two distinct JavaScript
     * entities, called "JS class" and "class metaobject",
     * respectively.
     *
     * The JS class is a usual JavaScript constructor function used to
     * construct instances.  It can also be used for instanceof
     * checks.
     *
     * The class metaobject is a Lisp object itself, and used to
     * represent the class on the Lisp side.
     *
     * The two entities making up a Lisp class are bidirectionally
     * linked, so having a reference to one lets you get the other:
     * The prototype.lisp_class of a JS class points to the class
     * metaobject; the js_class member of a class metaobject points to
     * the JS class.
     *
     * Rationale
     *
     * We want to create Lisp objects with normal constructor
     * functions, because these are presumably optimized by JS
     * engines. They also let us do instanceof checks.  Having a named
     * constructor function also aids with debugging as the name is
     * displayed in some tools (e.g. the Chrome console) when looking
     * at an object.
     *
     * At the same time, we want to have true class metaobjects on the
     * Lisp side.  If we used just JS classes, these would appear as
     * functions in Lisp.  By splitting a Lisp class into two pieces,
     * and linking them, we can get both normal JS classes in JS and
     * real Lisp classes in Lisp.
     */

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
        add_method(name, method)
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
        find_method(name)
        {
            const key = this.method_key(name);
            const method = this.get_js_class().prototype[key];
            if (method !== undefined)
                return method;
            else
                throw new vm.Unbound_method_error(this, name);
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

    /*
     * Change an existing standard class's superclass.
     */
    vm.reinitialize_standard_class = (lisp_class, lisp_super) =>
    {
        vm.assert_type(lisp_class, vm.Standard_class);
        vm.assert_type(lisp_super, vm.Standard_class);
        const js_class = lisp_class.get_js_class();
        const js_super = lisp_super.get_js_class();
        Object.setPrototypeOf(js_class.prototype, js_super.prototype);
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

            if (typeof(message) === "string")
                this.lisp_slot_message = vm.str(message);
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
            super("Type assertion failed: expected "
                  + vm.write_to_js_string(expected_type)
                  + " got "
                  + vm.write_to_js_string(datum));
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
     * Signalled when a method cannot be found in a class.
     */
    vm.Unbound_method_error = class Lisp_unbound_method_error extends vm.Error
    {
        constructor(cls, method_name)
        {
            vm.assert_type(cls, vm.Class);
            vm.assert_type(method_name, vm.Symbol);

            const name = method_name.get_string().to_js_string();
            super(`Unbound method: ${name}`);

            this.lisp_slot_class = cls;
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

;// CONCATENATED MODULE: ./tool/repl/repl-stream.mjs
/*
 * REPL Input Streams
 * Copyright (c) 2021 Manuel Simoni
 */

/*
 * Special module for treating asynchronous line based input as
 * synchronous.  See companion code in repl-stream.lispx
 */
function init_repl_stream(vm)
{
    /*
     * A REPL input buffer asynchronously receives lines from the
     * user.  But on the Lisp side, we want to have a blocking,
     * synchronous READ function so we can write our REPL in direct
     * style, as +DEITY+ intended.
     *
     * Here's how we do it:
     *
     * Every time a new line of data arrives, a wake-up function set
     * by Lisp is called.  It contains a saved continuation so Lisp
     * knows where to continue again.
     *
     * Lisp then obtains a fresh string input stream that contains the
     * current contents of the input buffer.
     *
     * Once an object has been successfully read, the input buffer
     * is truncated by the amount of data read from the stream.
     *
     * But if an object can't be read because there isn't enough data in
     * the buffer yet, Lisp saves the continuation in the wake-up
     * function again, and we start over.
     *
     * Note that we set an instance of this input buffer class as
     * standard input on the Lisp side, even though it doesn't even
     * implement vm.Input_stream!  But it's no problem, because we
     * don't expose methods like READ-BYTE to Lisp yet, just READ.
     * And READ internally calls the STREAM-READ method, which we
     * specialize for the input buffer class.
     */
    vm.REPL_input_buffer = class REPL_input_buffer extends vm.Object
    {
        /*
         * Construct a new input buffer.
         */
        constructor()
        {
            super();
            this.buffer = "";
            this.wake_up_function = null;
        }

        /*
         * Set the Lisp function that will be called when new data
         * arrives.  Called by Lisp.
         */
        set_wake_up_function(wake_up_function)
        {
            vm.assert_type(wake_up_function, vm.Function);
            if (this.wake_up_function === null) {
                this.wake_up_function = wake_up_function;
            } else {
                /*
                 * This happens if a coroutine tries to read while
                 * some other code is already waiting for input.
                 */
                vm.panic(new vm.Error("Concurrent access detected"));
            }
        }

        /*
         * Add a new line to the input buffer.  Notify Lisp via the
         * wake-up function.
         */
        add_line(line)
        {
            vm.assert_type(line, vm.String);
            this.buffer += line.get_utf8_bytes();
            if (this.wake_up_function !== null) {
                /*
                 * Reset wake-up function to null so we never
                 * accidentally enter the saved continuation twice.
                 */
                const wuf = this.wake_up_function;
                this.wake_up_function = null;
                vm.eval_form(vm.list(wuf));
            } else {
                vm.panic(new vm.Error("No wakeup function - Lisp crashed?"));
            }
        }

        /*
         * Get a fresh input stream that reads the current input
         * buffer contents.  Called by Lisp.
         */
        get_input_stream()
        {
            return new vm.String_input_stream(new vm.String(this.buffer));
        }

        /*
         * Remove the bytes that have been successfully read via the
         * input stream from the front of the buffer.  Called by Lisp
         * once it has read an object.
         */
        truncate_input_buffer(input_stream)
        {
            vm.assert_type(input_stream, vm.String_input_stream);
            vm.assert(input_stream.pos > 0);
            this.buffer = this.buffer.slice(input_stream.pos + 1);
        }

        /*
         * Remove all bytes from of the buffer.  Called by Lisp
         * when it gets a READER-ERROR.
         */
        clear_input_buffer()
        {
            this.buffer = "";
        }
    };

    /*
     * Lisp API
     */

    vm.define_class("repl:input-buffer", vm.REPL_input_buffer);

    vm.define_alien_function("repl:%set-input-buffer-wake-up-function",
                             (buffer, fun) => buffer.set_wake_up_function(fun));

    vm.define_alien_function("repl:%make-input-buffer-stream",
                             (buffer) => buffer.get_input_stream());

    vm.define_alien_function("repl:%truncate-input-buffer",
                             (buffer, stream) => buffer.truncate_input_buffer(stream));

    vm.define_alien_function("repl:%clear-input-buffer",
                             (buffer, stream) => buffer.clear_input_buffer(stream));

}

;// CONCATENATED MODULE: ./tool/repl/repl-stream.lispx
/* harmony default export */ const repl_stream_lispx = ("(defmethod stream-read ((input-buffer repl:input-buffer) . #ignore) (block exit (loop ((block trampoline (let ((stream (repl:%make-input-buffer-stream input-buffer))) (handler-case ((end-of-file (lambda #ignore (return-from trampoline (lambda () (take-subcont +root-prompt+ k (repl:%set-input-buffer-wake-up-function input-buffer (lambda () (push-delim-subcont +root-prompt+ k)))))))) (reader-error (lambda (e) (repl:%clear-input-buffer input-buffer) (error e)))) (let ((form (read stream))) (repl:%truncate-input-buffer input-buffer stream) (return-from exit form)))))))))");
;// CONCATENATED MODULE: ./tool/repl/repl.lispx
/* harmony default export */ const repl_lispx = ("(defconstant repl:+environment+ (the-environment) \"The environment in which REPL expressions are evaluated.\") (defdynamic repl:*debug-level* 0) (defun repl:run () (loop (restart-case ((repl-abort (lambda ()))) (repl:%display-prompt (dynamic repl:*debug-level*)) (fresh-line) (print (eval (read) repl:+environment+))))) (defun repl:run-debugger-loop (condition k) (uprint \"> Debugger invoked on condition:\") (print condition) (uprint \"> Available restarts -- use (invoke-restart 'name ...) to invoke:\") (mapc (lambda (restart) (unless (eq (slot-value restart (quote restart-name)) (quote repl-abort)) (print (slot-value restart (quote restart-name))))) (compute-restarts condition)) (uprint \"> Backtrace:\") (%print-stacktrace k) (repl:run)) (defun invoke-debugger (condition) (take-subcont +root-prompt+ k (push-delim-subcont +root-prompt+ k (dynamic-let ((repl:*debug-level* (+ (dynamic repl:*debug-level*) 1))) (restart-case ((abort (lambda () (invoke-restart (quote repl-abort))))) (typecase condition (unbound-symbol-error (let ((symbol (slot-value condition (quote symbol))) (env (slot-value condition (quote environment)))) (restart-case ((continue (lambda () (eval symbol env)) :associated-conditions (list condition)) (use-value (lambda (value) value) :associated-conditions (list condition)) (store-value (lambda (value) (eval (list #'def symbol value) env)) :associated-conditions (list condition))) (repl:run-debugger-loop condition k)))) (object (repl:run-debugger-loop condition k))))))))");
;// CONCATENATED MODULE: ./tool/repl/node/repl.mjs







/*
 * Init the VM and load the REPL code.
 */

const PROMPT = "* ";

var vm = new VM();
init_repl_stream(vm);

/*
 * Set up Lisp standard output to print to the Node stdout.
 */

vm.STANDARD_OUTPUT.set_value(new vm.JS_console_output_stream(function (output) {
    process.stdout.write(output + "\n");
}));

/*
 * Set up Lisp standard input to come from the input buffer.
 */

var rl = external_readline_namespaceObject.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: PROMPT
});

const input_buffer = new vm.REPL_input_buffer();
vm.STANDARD_INPUT.set_value(input_buffer);

rl.on("line", function(line) {
    input_buffer.add_line(vm.str(line + "\n"));
    /*
     * Force all output after each evaluation.
     */
    vm.STANDARD_OUTPUT.get_value().force_output();
});

/*
 * Run the REPL.
 */

vm.define_alien_function("repl:%display-prompt", (level) => {
    const lvl = vm.assert_type(level, vm.Number).to_js_number();
    if (lvl === 0) rl.setPrompt(PROMPT);
    else rl.setPrompt("[" + lvl + "] ");
    rl.prompt();
});

process.stdout.write("Welcome to Nybble Lisp!\n");

vm.eval_js_string(repl_lispx);
vm.eval_js_string(repl_stream_lispx);
vm.eval_js_string("(repl:run)");

/******/ })()
;