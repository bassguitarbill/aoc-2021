class Node {
  cameFrom = null;
  shortestPathCost = 999999999;
  constructor(x, y, risk, dist) {
    this.x = x;
    this.y = y;
    this.risk = risk;
    this.distToGoal = dist;
  }

  get cost() {
    return this.risk + this.distToGoal;
  }

  addNeighbors() {
    const neighbors = [];
    if (this.left  !== this.cameFrom) neighbors.push(this.addNeighbor(this.left));
    if (this.right !== this.cameFrom) neighbors.push(this.addNeighbor(this.right));
    if (this.up    !== this.cameFrom) neighbors.push(this.addNeighbor(this.up));
    if (this.down  !== this.cameFrom) neighbors.push(this.addNeighbor(this.down));
    return neighbors.filter(x => x);
  }

  addNeighbor(n) {
    if (n) {
      const cost = this.shortestPathCost + n.risk;
      if (cost < n.shortestPathCost) {
        n.shortestPathCost = cost;
        n.cameFrom = this;
      }
    }
    return n;
  }
}

fetch('data/15-data.txt').
  then(r => r.text()).
  then(generateNodes).
  then(buildNodeReferences).
  then(drawNodes).
  then(console.log);

function generateNodes(str) {
  document.querySelector('#step').addEventListener('click', () => {
    const goal = findShortestPath();
    drawNodes();
    if (goal) console.log(goal)
  });
  document.querySelector('#play').addEventListener('click', () => {
    let goal;
    while (!goal) {
      goal = findShortestPath();
      drawNodes();
    }
    console.log(goal);
  });
  const nodeList = [];
  const nodes = [];
  const lines = str.split('\n').slice(0, -1);
  const height = lines.length;
  const width = lines[0].length;
  for (let yPlus = 0; yPlus < 5; yPlus++) {
    for (let y = 0; y < height; y++) {
      nodes.push([]);
      for (let xPlus = 0; xPlus < 5; xPlus++) {
        for (let x = 0; x < width; x++) {
          const realX = (xPlus * width) + x;
          const realY = (yPlus * height) + y;
          nodes[realY].push(new Node(realX, realY, normalize(Number(lines[y][x]) + xPlus + yPlus), ((5 * width) - realX - 1) + ((5 * height) - realY - 1)));
          nodeList.push(nodes[realY][realX]);
        }
      }
    }
  }
  window.openSet = nodeList;
  window.openSet.unshift();
  nodes[0][0].shortestPathCost = 0;
  return nodes;
}

function normalize(n) {
  while (n > 9) n -= 9;
  return n;
}

function drawNodes() {
  return;
  let caveText = "";
  for (let y=0; y < nodes.length; y++) {
    caveText = caveText + "<div>";
    for (let x=0; x < nodes[y].length; x++) {
      const node = nodes[y][x];
      if (node == window.nextNode) {
        caveText = caveText + `<span style="color: red">${node.risk}</span>`;
      } else if (node.shortestPathCost < 999999999) {
        caveText = caveText + `<span style="color: green">${node.risk}</span>`;
      } else {
        caveText = caveText + `<span style="color: black">${node.risk}</span>`;
      }
    }
    caveText = caveText + "</div>";
  }
  document.querySelector('#cave').innerHTML = caveText;
  return nodes;
}

function buildNodeReferences(nodes) {
  const height = nodes.length;
  const width = nodes[0].length;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (x > 0) nodes[y][x].left = nodes[y][x - 1];
      if (x < width - 1) nodes[y][x].right = nodes[y][x + 1];
      if (y > 0) nodes[y][x].up = nodes[y - 1][x];
      if (y < height - 1) nodes[y][x].down = nodes[y + 1][x];
    }
  }
  window.nodes = nodes;
}

function findShortestPath() {
  const openSet = window.openSet;
  const nextNode = openSet.shift();
  window.nextNode = nextNode;
  nextNode.addNeighbors();
  openSet.sort((a, b) => a.shortestPathCost - b.shortestPathCost);
  if ((openSet.length % 1000) === 0) console.log(openSet.length);
  if (openSet[0].distToGoal === 0) return openSet[0];
}

function cull(os) {
  return os.filter((n, i) => i === os.indexOf(n)).filter(n => n.shortestPathCost === 999999999);
}
  




