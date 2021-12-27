window.onload = () => {
  document.querySelector('#submit').addEventListener('click', () => {
    const hex = fetchHex();
    const binString = populateBinary(hex);
    const [packet, rem] = parsePacket(binString);
    console.log(packet);
    // console.log(versionSum(packet));
    console.log(packetValue(packet));
  });

  function fetchHex() {
    return document.querySelector('#in').value;
  }


  /* function populateBinary(hex) {
    const num = parseInt(hex, 16);
    let pk = Number(num).toString(2);
    while (pk.length % 4 !== 0) pk = '0' + pk;
    document.querySelector('#binary').innerText = pk;
    return pk;
  } */
}

function populateBinary(hex) {
  let pk = '';
  while (hex.length > 4) {
    const chunk = hex.substring(hex.length - 4, hex.length);
    let chunkStr = Number(parseInt(chunk, 16)).toString(2);
    while (chunkStr.length % 16 !== 0) chunkStr = '0' + chunkStr;
    pk = chunkStr + pk;
    hex = hex.substring(0, hex.length - 4);
  }
  pk = Number(parseInt(hex, 16)).toString(2) + pk;
  while (pk.length % 4 !== 0) pk = '0' + pk;
  return pk;
}


function parsePacket(binString) {
  let pk = binString;
  const packet = {};

  const version = pk.substring(0, 3);
  packet.version = parseInt(version, 2);
  pk = pk.substring(3);

  const typeId = pk.substring(0, 3);
  packet.typeId = parseInt(typeId, 2);
  pk = pk.substring(3);

  if (packet.typeId === 4) {
    packet.type = 'literal'

    let numString = '';
    while (pk.startsWith('1')) {
      numString = numString + pk.substring(1, 5);
      pk = pk.substring(5);
    }
    numString = numString + pk.substring(1, 5);
    pk = pk.substring(5);

    packet.number = parseInt(numString, 2);

  } else {
    packet.type = 'operator';

    let lengthTypeId = pk.substring(0, 1);
    packet.lengthTypeId = parseInt(lengthTypeId, 2);
    pk = pk.substring(1);

    packet.subPackets = [];

    if (packet.lengthTypeId === 0) {
      const subPacketLength = pk.substring(0, 15);
      packet.subPacketLength = parseInt(subPacketLength, 2);
      pk = pk.substring(15);

      let subPackets = pk.substring(0, packet.subPacketLength);
      pk = pk.substring(packet.subPacketLength);

      while (isValidPacket(subPackets)) {
        const pktAndRem = parsePacket(subPackets);
        packet.subPackets.push(pktAndRem[0]);
        subPackets = pktAndRem[1];
      }
    } else if (packet.lengthTypeId === 1) {
      const numberOfSubpackets = pk.substring(0, 11);
      packet.numberOfSubpackets = parseInt(numberOfSubpackets, 2);
      pk = pk.substring(11);

      for (let i=0; i<packet.numberOfSubpackets; i++) {
        const pktAndRem = parsePacket(pk);
        packet.subPackets.push(pktAndRem[0]);
        pk = pktAndRem[1];
      }
    } else {
      throw ('Invalid length ID ' + packet.lengthTypeId)
    }
  }

  return [packet, pk];
}

function isValidPacket(bs) {
  return bs.length > 6;
}

function versionSum(packet) {
  if (packet.type === 'literal') {
    return packet.version;
  } else {
    return packet.subPackets.reduce((acc, sp) => acc + versionSum(sp), packet.version);
  }
}

function packetValue(packet) {
  if (packet.type === 'literal') {
    return packet.number;
  } else {
    if (packet.typeId === 0) { // addition
      return packet.subPackets.reduce((acc, sp) => acc + packetValue(sp), 0);
    }
    if (packet.typeId === 1) { // multiplication
      return packet.subPackets.reduce((acc, sp) => acc * packetValue(sp), 1);
    }
    if (packet.typeId === 2) { // minimum
      return packet.subPackets.reduce((acc, sp) => Math.min(acc, packetValue(sp)), Infinity);
    }
    if (packet.typeId === 3) { // maximum
      return packet.subPackets.reduce((acc, sp) => Math.max(acc, packetValue(sp)), -Infinity);
    }
    if (packet.typeId === 5) { // greater than
      return packetValue(packet.subPackets[0]) > packetValue(packet.subPackets[1]) ? 1 : 0;
    }
    if (packet.typeId === 6) { // less than
      return packetValue(packet.subPackets[0]) < packetValue(packet.subPackets[1]) ? 1 : 0;
    }
    if (packet.typeId === 7) { // equal to
      return packetValue(packet.subPackets[0]) ===  packetValue(packet.subPackets[1]) ? 1 : 0;
    }
    throw 'Bad packet ID ' + packet.typeId;
  }
}


