export const unsafeInsertAtImpl = function (i, a, l) {
  var l1 = l.slice();
  l1.splice(i, 0, a);
  return l1;
};

export const unsafeInsertArrayImpl = function (i, a, l) {
  var l1 = l.slice();
  l1.splice.apply(l1, [i, 0].concat(a));
  return l1;
};

export const unsafeDeleteAtImpl = function (i, l) {
  var l1 = l.slice();
  l1.splice(i, 1);
  return l1;
};

export const unsafeUpdateAtImpl = function (i, a, l) {
  var l1 = l.slice();
  l1[i] = a;
  return l1;
};

export const unsafeModifyAtImpl = function (i, f, l) {
  var l1 = l.slice();
  l1[i] = f(l1[i]);
  return l1;
};
