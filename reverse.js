module.exports = function(xs) {
  return xs.reduce(function(accum, x) {
    return [x].concat(accum);
  }, []);
};
