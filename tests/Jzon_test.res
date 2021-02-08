open Test

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

test("Equals", () => {
  let a = 1
  let b = 1
  intEqual(a, b)
})
