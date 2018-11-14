/** @format */

const localStorage = window.localStorage

export default {
  add(oi) {
    var orders = JSON.parse(localStorage.getItem('orders') || '[]')
    orders.push(oi)
    localStorage.setItem('orders', JSON.stringify(orders))
  },

  remove(oi) {
    var orders = JSON.parse(localStorage.getItem('orders') || '[]')
    let i = orders.indexOf(oi)
    if (i === -1) return
    orders.splice(i, 1)
    localStorage.setItem('orders', JSON.stringify(orders))
  },

  list() {
    return JSON.parse(localStorage.getItem('orders') || '[]')
  }
}
