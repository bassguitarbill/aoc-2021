const xmin =   85;
const xmax =  145;
const ymin = -163;
const ymax = -108;

function doesHit(x, y, xvel, yvel) {
  if (x >= xmin && x <= xmax && y >= ymin && y <= ymax) {
    return true;
  } else if (x > xmax || y < ymin) {
    return false;
  } else if (xvel === 0) {
    return doesHit(x + xvel, y + yvel, xvel, yvel - 1);
  } else {
    return doesHit(x + xvel, y + yvel, xvel - 1, yvel - 1);
  }
}

let hits = 0;
for (let i = 0; i < 146; i++) {
  for (let j = -164; j < 164; j++) {
    if (doesHit(0, 0, i, j)) hits++;
  }
}

console.log(hits);
