class Cave {
  name;
  isBig;
  isStart;
  isEnd;
  connections = [];

  constructor(name) {
    this.name = name;
    this.isBig = name === name.toUpperCase();
    this.isStart = name === "start";
    this.isEnd = name === "end";
  }
}

class Path {
  valid;
  valid2;
  constructor(caveSystem, caveList) {
    this.caveSystem = caveSystem;
    this.caveList = caveList;
    this.caveListString = JSON.stringify(this.caveList);
  }

  isValid() {
    if(this.valid === true || this.valid === false) return this.valid;
    const c = this.counts();
    this.valid = !Object.keys(c).find(count => !this.caveSystem[count].isBig && c[count] > 1);
    return this.valid;
  }

  isValid2() {
    if(this.valid2 === true || this.valid2 === false) return this.valid2;
    const c = this.counts();
    const smallCounts = Object.keys(c).filter(count => !this.caveSystem[count].isBig);
    const smallVisitedMoreThanOnce = smallCounts.filter(count => c[count] > 1);
    if (smallVisitedMoreThanOnce.length === 0) this.valid2 = true;
    else if (smallVisitedMoreThanOnce.length > 1) this.valid2 = false;
    else if (smallVisitedMoreThanOnce[0] === 'start' || smallVisitedMoreThanOnce[0] === 'end') this.valid2 = false;
    else if (c[smallVisitedMoreThanOnce[0]] > 2) this.valid2 = false;
    else this.valid2 = true;
    debugger;
    return this.valid2;
  }



  counts() {
    const caveCounts = {};
    this.caveList.forEach(c => {
      caveCounts[c] = caveCounts[c] ? caveCounts[c] + 1 : 1;
    });
    return caveCounts;
  }

  generateNewPaths() {
    if(this.caveList[this.caveList.length - 1] === 'end') return [];
    return this.caveSystem[this.caveList[this.caveList.length - 1]].connections.map(c => {
      return this.caveList.concat(c.name);
    });
  }
}

//fetch('data/12-sample01.txt').then(r => r.text()).then(generateCaves).then(generatePaths).then(console.log);
// fetch('data/12-sample02.txt').then(r => r.text()).then(generateCaves).then(generatePaths).then(console.log);
//fetch('data/12-sample03.txt').then(r => r.text()).then(generateCaves).then(generatePaths).then(console.log);
fetch('data/12-data.txt').then(r => r.text()).then(generateCaves).then(generatePaths).then(console.log);

function generateCaves(caveString) {
  const caveSystem = {};
  caveString.split('\n').forEach(l => {
    if (!l) return;
    [f, s] = l.split('-');
    const firstCave = caveSystem[f] ? caveSystem[f] : caveSystem[f] = new Cave(f);
    const secondCave = caveSystem[s] ? caveSystem[s] : caveSystem[s] = new Cave(s);
    firstCave.connections.push(secondCave);
    secondCave.connections.push(firstCave);
  });
  return caveSystem;
}

function generatePaths(caveSystem, paths = []) {
  if (paths.length === 0) return generatePaths(caveSystem, [new Path(caveSystem, ['start'])]);
  const newPaths = [];
  paths.forEach(p => {
    p.generateNewPaths().forEach(px => {
      const newPath = new Path(caveSystem, px);
      if (newPath.isValid2()) newPaths.push(newPath);
    });
  });
  const newPathsNoDupes = newPaths.filter(p => !paths.find(p2 => p.caveListString === p2.caveListString));
  if (newPathsNoDupes.length === 0) return paths.concat(newPathsNoDupes).filter(p => {
    return p.caveList[p.caveList.length - 1] === 'end';
  });
  console.log(newPathsNoDupes.length);
  return generatePaths(caveSystem, paths.concat(newPathsNoDupes));
}

