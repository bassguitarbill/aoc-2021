const fs = require('fs');

class Node {
  constructor(str, par) {
    this.par = par;
    str = str.substring(1, str.length - 1);
    const leftStart = 0;
    let leftEnd;
    let depth = 0;
    for (let i=0; i<str.length; i++) {
      if (str[i] === '[') depth++;
      else if (str[i] === ']') depth--;
      else if (str[i] === ',' && depth === 0) leftEnd = i;
    }

    const left = str.substring(leftStart, leftEnd);
    if (!isNaN(Number(left))) this.left = Number(left);
    else this.left = new Node(left, this);

    const right = str.substring(leftEnd + 1);
    if (!isNaN(Number(right))) this.right = Number(right);
    else this.right = new Node(right, this);
  }

  isRoot() {
    return !par;
  }

  toString() {
    return `[${this.left.toString()},${this.right.toString()}]`;
  }

  get depth() {
    if (this.par) return this.par.depth + 1;
    return 0;
  }

  get isRight() {
    return this.par && this.par.right === this;
  }

  get isLeft() {
    return this.par && this.par.left === this;
  }

  explode() {
    if (this.left.depth > 3) {
      if (!isNaN(Number(this.right))) this.right += this.left.right;
      else this.right.addDownLeft(this.left.right);
      this.addUpLeft(this.left.left);
      this.left = 0;
      return true;
    } else if (this.right.depth > 3) {
      if (!isNaN(Number(this.left))) this.left += this.right.left;
      else this.left.addDownRight(this.right.left);
      this.addUpRight(this.right.right);
      this.right = 0;
      return true;
    } else if (this.left.isLeft && this.left.explode()) {
      return true;
    } else if (this.right.isRight && this.right.explode()) {
      return true;
    }
    return false;
  }

  addUpRight(num) {
    if (this.isLeft) {
      if (!isNaN(Number(this.par.right))) this.par.right += num;
      else this.par.right.addDownLeft(num);
    } else if (this.par) {
      this.par.addUpRight(num);
    }
  }

  addDownRight(num) {
    if (!isNaN(Number(this.right))) this.right += num;
    else this.right.addDownRight(num);
  }

  addUpLeft(num) {
    if (this.isRight) {
      if (!isNaN(Number(this.par.left))) this.par.left += num;
      else this.par.left.addDownRight(num);
    } else if (this.par) {
      this.par.addUpLeft(num);
    }
  }

  addDownLeft(num) {
    if (!isNaN(Number(this.left))) this.left += num;
    else this.left.addDownLeft(num);
  }

  split() {
    return this.splitLeft() || this.splitRight();
  }

  splitLeft() {
    if (isNaN(Number(this.left))) return this.left.split();
    else if (this.left > 9) {
      const l = Math.floor(this.left / 2);
      const r = Math.ceil(this.left / 2);
      this.left = new Node(`[${l},${r}]`, this);
      return true;
    } else return false;
  }

  splitRight() {
    if (isNaN(Number(this.right))) return this.right.split();
    else if (this.right > 9) {
      const l = Math.floor(this.right / 2);
      const r = Math.ceil(this.right / 2);
      this.right = new Node(`[${l},${r}]`, this);
      return true;
    } else return false;
  }

  reduce() {
    while (true) {
      if (this.explode()) { console.log(`After explode: ${this.toString()}`); continue; }
      if (this.split()) { console.log(`After split: ${this.toString()}`); continue; }
      break;
    }
  }

  magnitude() {
    const lmag = isNaN(Number(this.left)) ? this.left.magnitude() : this.left;
    const rmag = isNaN(Number(this.right)) ? this.right.magnitude() : this.right;
    return (3 * lmag) + (2 * rmag);
  }
}

function add(n1, n2) {
  const nodeStr = `[${n1.toString()},${n2.toString()}]`;
  return new Node(nodeStr);
}


const data = fs.readFileSync('data/18-data.txt', 'utf-8');
const lines = data.trim().split('\n');
let sum;

for (let x=0; x < lines.length; x++) {
  if (!sum) sum = new Node(lines[x]);
  else sum = add(sum, new Node(lines[x]));
  sum.reduce();
}

console.log(sum.magnitude());

let max = 0;
for (let x=0; x < lines.length; x++) {
  for (let y=0; y < lines.length; y++) {
    if (x === y) continue;
    const rslt = add(new Node(lines[x]), new Node(lines[y]));
    rslt.reduce();
    max = Math.max(max, rslt.magnitude());
  }
}
console.log(max);

