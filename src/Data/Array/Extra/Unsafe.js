"use strict";

export const unsafeInsertAtImpl = function (i) {
  return function (a) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 0, a);
      return l1;
    };
  };
};

export const unsafeInsertArrayImpl = function (i) {
  return function (a) {
    return function (l) {
      return l.splice.apply(l, [i, 0].concat(a));
    };
  };
};

export const unsafeDeleteAtImpl = function (i) {
  return function (l) {
    var l1 = l.slice();
    l1.splice(i, 1);
    return l1;
  };
};

export const unsafeUpdateAtImpl = function (i) {
  return function (a) {
    return function (l) {
      var l1 = l.slice();
      l1[i] = a;
      return l1;
    };
  };
};

export const unsafeModifyAtImpl = function (i) {
  return function (f) {
    return function (l) {
      var l1 = l.slice();
      l1[i] = f(l1[i]);
      return l1;
    };
  };
};
