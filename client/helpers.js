/** @format */

export function uniq(arr) {
  var map = {}
  var newarr = []
  for (let i = 0; i < arr.length; i++) {
    let elem = arr[i]
    if (elem in map) continue
    newarr.push(elem)
    map[elem] = true
  }
  return newarr
}
