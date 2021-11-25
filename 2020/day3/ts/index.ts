import fs from "fs";

const input = fs.readFileSync("input.txt", "utf8");

const format1 = input.split("\r\n");
const format2 = format1.map((k) => k.split(""));

// console.log(format2);

// This is the initial solution for the first part, but its not required anymore
// since the solution for part 2 is a lot cleaner and it also works on the first part

// const solve1 = () => {
//   let s = 0;
//   let i = 0;
//   let k = 0;

//   while (i < format2.length) {
//     if (format2[i][k] === "#") s++;

//     k += 3;
//     k %= format2[i].length;
//     i++;
//   }
//   console.log(s);
// };

const solve2 = (ymultiplier: number, xmultiplier: number) => {
  let s = 0;
  let currentY = 0;
  let currentX = 0;

  while (currentY < format2.length) {
    if (format2[currentY][currentX] === "#") s++;
    currentX += xmultiplier;
    currentX %= format2[currentY].length;
    currentY += ymultiplier;
  }
  return s;
};

// console.log(solve2(1,3))  //part1
console.log(
  solve2(1, 1) * solve2(1, 3) * solve2(1, 5) * solve2(1, 7) * solve2(2, 1)
); // part2
