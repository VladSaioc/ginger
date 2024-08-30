c1 = chan [k1];
c2 = chan [k2];

go {
  for i1 : 0 .. x1 {
    c1!;
  };
  for i2 : 0 .. y1 {
    c2?;
  };
  for i3 : 0 .. z1 {
    c1!;
  }
};

go {
  for j1 : 0 .. x2 {
    c1?;
  };
};

for j2 : 0 .. y2 {
  c2!;
};
