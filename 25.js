window.onload = function() {
  document.querySelector('#upload').addEventListener('change', upload);
  document.querySelector('#step').addEventListener('click', step);
  document.querySelector('#play').addEventListener('click', playOut);
}

let width  = 0;
let height = 0;

let steps = 0;

let rightCukes = [];
let downCukes  = [];
let cukes = [[]];

function upload(e) {
  const r = new FileReader();
  r.addEventListener('load', f => {
    const result = f.target.result;
    document.querySelector('#cukefield').innerText = result;
    parseInput(result);
  });
  r.readAsText(e.target.files[0]);
}

function parseInput(input) {
  const rows = input.split('\n');
  cukes = [];
  rows.forEach(r => {
    if(r.length) cukes.push(r.split(''))
  });
  height = cukes.length;
  width = cukes[0].length;
  rightCukes = [];
  downCukes = [];
  for(let i=0; i<height; i++) {
    for(let j=0; j<width; j++) {
      const c = cukes[i][j];
      if(c === ">") rightCukes.push([i, j]);
      else if(c === "v") downCukes.push([i, j]);
    }
  }
}

function step() {
  let moveHappened = false;
  const nextRightCukes = [];
  rightCukes.forEach(c => {
    [x, y] = c;
    ty = (y + 1) % width;
    if(cukes[x][ty] === ".") {
      nextRightCukes.push([x, ty]);
      moveHappened = true;
    } else {
      nextRightCukes.push(c);
    }
  });
  rightCukes = nextRightCukes;
  regenerateCukes();

  const nextDownCukes = [];
  downCukes.forEach(c => {
    [x, y] = c;
    tx = (x + 1) % height;
    if(cukes[tx][y] === ".") {
      moveHappened = true;
      nextDownCukes.push([tx, y]);
    } else {
      nextDownCukes.push(c);
    }
  });
  downCukes = nextDownCukes;
  regenerateCukes();

  steps++;

  redrawCukes();
  return moveHappened;
}

function regenerateCukes() {
  cukes = [];
  for(let i=0; i<height; i++) {
    cukes.push([]);
    for(let j=0; j<width; j++) {
      cukes[i].push(".");
    }
  }
  rightCukes.forEach(c => {
    [x, y] = c;
    cukes[x][y] = ">";
  });
  downCukes.forEach(c => {
    [x, y] = c;
    cukes[x][y] = "v";
  });
}

function redrawCukes() {
  let cukeString = "";
  cukes.forEach(r => {
    r.forEach(c => {
      cukeString += c;
    });
    cukeString += '\n';
  });
  document.querySelector('#cukefield').innerText = cukeString;
  document.querySelector('#steps').innerText = steps;
}

function playOut() {
  //while(step()){}
  const interval = window.setInterval(() => {
    const done = step();
    if (!done) clearInterval(interval);
  }, 33);
}
