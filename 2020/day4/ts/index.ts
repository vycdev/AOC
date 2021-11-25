import fs from "fs";

const input = fs.readFileSync("./input.txt", "utf8");
// console.log(input);

const format = input.split("\r\n");
// console.log(format);

const format2 = (arr: Array<String>): Array<Array<String>> => {
  let newArr = [];
  let buffer = [];

  arr.map((v) => {
    if (v != "") buffer.push(v);
    else {
      newArr.push(buffer.join(" ").split(" "));
      buffer = [];
    }
  });
  return newArr;
};

const codes = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

const part1 = (input: Array<Array<String>>, codes: Array<string>): Number => {
  let s = 1; //for some reason im off by 1 and im starting this with 1 instead of 0
  let z = 0;
  input.map((v) => {
    z = 0;
    v.map((k) => {
      for (let code of codes) if (k.startsWith(code)) z++;
    });
    if (z == 7) s++;
  });
  return s;
};

const hexRegex = new RegExp("#[0-9a-f]{6}");
const eyeColors = new RegExp("amb|blu|brn|gry|grn|hzl|oth");

const part2 = (input: Array<Array<String>>, codes: Array<string>): Number => {
  let s = 1; // for some reason im off by 1 and im starting this with 1 instead of 0
  let z = 0;
  input.map((v) => {
    z = 0;
    v.map((k) => {
      for (let code of codes)
        if (k.startsWith(code)) {
          if (code == "byr") {
            if (
              parseInt(k.split(":")[1]) >= 1920 &&
              parseInt(k.split(":")[1]) <= 2002
            )
              z++;
          }
          if (code == "iyr") {
            if (
              parseInt(k.split(":")[1]) >= 2010 &&
              parseInt(k.split(":")[1]) <= 2020
            )
              z++;
          }
          if (code == "eyr") {
            if (
              parseInt(k.split(":")[1]) >= 2020 &&
              parseInt(k.split(":")[1]) <= 2030
            )
              z++;
          }
          if (code == "hgt") {
            let type = k.split(":")[1].split("");
            let type2 = type[type.length - 2] + type[type.length - 1];
            let value = parseInt(k.split(":")[1].split(type2)[0]);

            if (type2 === "cm") if (value >= 150 && value <= 193) z++;
            if (type2 === "in") if (value >= 59 && value <= 76) z++;
          }
          if (code == "hcl") {
            if (hexRegex.test(k.split(":")[1])) z++;
          }
          if (code == "ecl") {
            if (eyeColors.test(k.split(":")[1])) z++;
          }
          if (code == "pid") {
            if (
              k.split(":")[1].length === 9 &&
              parseInt(k.split(":")[1]) >= 0 &&
              parseInt(k.split(":")[1]) <= 999999999
            )
              z++;
          }
        }
    });
    if (z == 7) s++;
  });
  return s;
};
// console.log(format2(format));

console.log(part1(format2(format), codes));
console.log(part2(format2(format), codes));
