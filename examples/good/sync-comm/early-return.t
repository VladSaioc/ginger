c1 = [0];
c2 = [0];
go {
  go {
    c1!
  };
  go {
    c2!
  };
  c1?;
  if b1 {
    return
  } else {
    skip
  };
  c2?;
}
