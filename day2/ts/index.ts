import fs from "fs";


const input = fs.readFileSync('input.txt', 'utf8')
// console.log(input);

const splitInput = input.split('\r\n')
// console.log(splitInput);

const formatInput = splitInput.map((v)=> v.split(': '))
const formatInput2 = formatInput.map((v)=> [v[0].split(' '), v[1]])
const formatInput3 = formatInput2.map((v)=> [[v[0][0].split('-').map(h => parseInt(h)), v[0][1]], v[1]])

const part1 = ()=> {
    let sum = 0;

    formatInput3.map(k => {
        const from = k[0][0][0]
        const to = k[0][0][1]

        const letter = k[0][1]
        const string = k[1]

        let s = 0;
        for(let i=0; i<=string.length; i++){
            if(string[i] === letter) s++;
        }
        if(s >= from && s <= to) sum ++;
    })

    return sum;
}

const part2 = ()=> {
    let sum = 0;

    formatInput3.map(k => {
        const from = k[0][0][0] as number
        const to = k[0][0][1] as number

        const letter = k[0][1]
        const string = k[1]

        if((letter === string[from-1]) !== (letter === string[to-1])) sum ++;
    })

    return sum;
}

console.log(part1());
console.log(part2());
