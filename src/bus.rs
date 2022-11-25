#[derive(Default)]
pub struct Bus {
    pub nmi_asserted: bool,
    pub frame_finished: bool,
}

impl Bus {
    pub fn new() -> Bus {
        Bus { ..Default::default() }
    }
    // pub fn set_vblank_nmi(&mut self, val: bool) {
    //     self.nmi_asserted = val;
    // }
    // pub fn get_vblank_nmi(&self) -> bool {
    //     self.nmi_asserted
    // }
}