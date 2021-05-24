self: super:
let chan = self.rustChannelOf { date = "2021-03-01"; channel = "nightly"; };
in {
  rustc = chan.rust;
  cargo = chan.rust;
}
