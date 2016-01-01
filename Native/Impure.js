var getRandom = function() {
  return Math.random()
};

var make = function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Impure = elm.Native.Impure || {};

    if (elm.Native.Impure.values) return elm.Native.Impure.values;

    return {
        'getRandom': getRandom
    };
};

Elm.Native.Impure = {};
Elm.Native.Impure.make = make;