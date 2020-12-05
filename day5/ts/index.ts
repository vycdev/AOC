import fs from 'fs'



const input = fs.readFileSync("input.txt", "utf8")

const format = input.split("\r\n")
// .map(a => a.split(""));

const part1 = (code:string,index:number, row:[number, number], seat:[number,number]):number=>{
    if(code[index] === 'F')return part1(code, index+1, [row[0], Math.floor(row[1]/2)], seat)
    else if(code[index]==='B')return part1(code, index+1, [Math.floor(row[1]/2), row[1]], seat)
    else if(code[index]==='R')return part1(code, index+1, row, [Math.floor(seat[1]/2), seat[1]])
    else if(code[index]==='L')return part1(code, index+1, row, [seat[0], Math.floor(seat[1]/2)])
    else return row[0]*8+seat[0]
}

const part1Answer = ()=> Math.max(...format.map(a=> part1(a, 0, [0,127], [0,7])))




console.log(part1Answer());
console.log(Math.floor(33/2)+1);
