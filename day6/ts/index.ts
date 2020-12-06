import fs from "fs"


const input = fs.readFileSync('input.txt', 'utf8')

const format = input.split("\r\n")
const formatForPart1 = (arr: Array<String>): Array<Array<String>> => {
    let newArr= [];
    let buffer= [];
  
    arr.map((v) => {
      if (v != "") buffer.push(v);
      else {
        newArr.push(buffer.join("").split(""));
        buffer = [];
      }
    });
    newArr.push(buffer.join("").split(""))
    return newArr;
  };
  
  const formatForPart2 = (arr: Array<String>): Array<Array<Array<String>>> => {
    let newArr= [];
    let buffer= [];
  
    arr.map((v) => {
      if (v != "") buffer.push(v);
      else {
        newArr.push(buffer.join(" ").split(" ").map(a=>a.split("")));
        buffer = [];
      }
    });
    newArr.push(buffer.join(" ").split(" ").map(a=>a.split("")))
    return newArr;
  };

const finalFormatPart1 = formatForPart1(format)
const finalFormatPart2 = formatForPart2(format)

const part1 = finalFormatPart1.map(a => a.filter((i, p, s) => s.indexOf(i) == p).length).reduce((a,b)=>a+b)
const part2 = finalFormatPart2.map(a=> a.reduce((a, b) => a.filter(c => b.includes(c))).length).reduce((a,b)=>a+b)


console.log(part1);
console.log(part2);

