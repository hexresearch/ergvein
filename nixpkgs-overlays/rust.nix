/* self: super:
let chan = self.rustChannelOf { date = "2021-03-01"; channel = "nightly"; };
in {
  rustc-nightly = chan.rust;
  cargo-nightly = chan.rust;
} */
self: super:
{
  rustc-nightly = self.fenix.complete.rustc;
  cargo-nightly = self.fenix.complete.cargo;
}
