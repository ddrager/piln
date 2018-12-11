/** @format */

const localStorage = window.localStorage

export default {
  add(oi) {
    let orders = JSON.parse(localStorage.getItem('orders') || '[]')
    var orderMap = makeMap(orders)
    orderMap[oi] = true
    localStorage.setItem('orders', JSON.stringify(Object.keys(orderMap)))
  },

  remove(oi) {
    let orders = JSON.parse(localStorage.getItem('orders') || '[]')
    var orderMap = makeMap(orders)
    delete orderMap[oi]
    localStorage.setItem('orders', JSON.stringify(Object.keys(orderMap)))
  },

  list() {
    let orders = JSON.parse(localStorage.getItem('orders') || '[]')
    let orderMap = makeMap(orders)
    return Object.keys(orderMap).sort()
  }
}

function makeMap(arr) {
  var map = {}
  for (let i = 0; i < arr.length; i++) {
    map[arr[i]] = true
  }
  return map
}
