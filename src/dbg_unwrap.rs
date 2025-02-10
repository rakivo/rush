pub trait DbgUnwrap<T> {
    fn unwrap_dbg(self) -> T;
}

impl<T> DbgUnwrap<T> for std::option::Option<T> {
    #[track_caller]
    #[inline(always)]
    fn unwrap_dbg(self) -> T {
        #[cfg(feature = "dbg")] { self.unwrap() }
        #[cfg(not(feature = "dbg"))] unsafe { self.unwrap_unchecked() }
    }
}

impl<T, E> DbgUnwrap<T> for std::result::Result<T, E>
where
    E: std::fmt::Debug
{
    #[track_caller]
    #[inline(always)]
    fn unwrap_dbg(self) -> T {
        #[cfg(feature = "dbg")] { self.unwrap() }
        #[cfg(not(feature = "dbg"))] unsafe { self.unwrap_unchecked() }
    }
}
