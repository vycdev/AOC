import fs from "fs";

const input = fs.readFileSync("input.txt", "utf8");

const format = input.split("\r\n");
// .map(a => a.split(""));

const calculateSeat = (
  code: string,
  index: number,
  row: [number, number],
  seat: [number, number]
): number => {
  if (code[index] === "F")
    return calculateSeat(
      code,
      index + 1,
      [row[0], Math.floor((row[1] - row[0]) / 2 + row[0])],
      seat
    );
  else if (code[index] === "B")
    return calculateSeat(
      code,
      index + 1,
      [Math.floor((row[1] - row[0]) / 2 + row[0] + 1), row[1]],
      seat
    );
  else if (code[index] === "R")
    return calculateSeat(code, index + 1, row, [
      Math.floor((seat[1] - seat[0]) / 2 + seat[0] + 1),
      seat[1],
    ]);
  else if (code[index] === "L")
    return calculateSeat(code, index + 1, row, [
      seat[0],
      Math.floor((seat[1] - seat[0]) / 2 + seat[0]),
    ]);
  else {
    // console.log(row,seat);
    return row[1] * 8 + seat[1];
  }
};

const part1Answer = () =>
  Math.max(...format.map((a) => calculateSeat(a, 0, [0, 127], [0, 7])));
const part2Answer = () => {
  let k = format.map((a) => calculateSeat(a, 0, [0, 127], [0, 7]));
  // console.log(k);
  let n = [];
  for (let i = 0; i < k.length; i++)
    for (let j = i + 1; j <= k.length; j++) {
      if (k[i] - k[j] == 2) n.push(k[i] - 1);
      if (k[i] - k[j] == -2) n.push(k[i] + 1);
    }
  n = n.filter((a) => {
    let z = true;
    for (let s of k) if (s == a) z = false;
    return z;
  });

  return n;
};

console.log(part1Answer());
console.log(part2Answer());
