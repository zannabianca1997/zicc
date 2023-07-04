pub trait CollectOksOrErrs {
    fn collect_oks_or_errs<Ts, Es, T, E>(self) -> Result<Ts, Es>
    where
        Self: Sized + IntoIterator<Item = Result<T, E>>,
        Ts: Default + Extend<T>,
        Es: Default + Extend<E>,
    {
        self.into_iter().fold(Ok(Ts::default()), |mut res, item| {
            match (&mut res, item) {
                (Ok(ts), Ok(t)) => ts.extend_one(t),
                (Ok(_), Err(e)) => {
                    let mut errs = Es::default();
                    errs.extend_one(e);
                    res = Err(errs)
                }
                (Err(_), Ok(_)) => (),
                (Err(es), Err(e)) => es.extend_one(e),
            }
            res
        })
    }
}
impl<T> CollectOksOrErrs for T {}
pub trait CollectOksOrFlatErrs {
    fn collect_oks_or_flat_errs<Ts, Esf, Esi, T, E>(self) -> Result<Ts, Esf>
    where
        Self: Sized + IntoIterator<Item = Result<T, Esi>>,
        Ts: Default + Extend<T>,
        Esf: Default + Extend<E>,
        Esi: IntoIterator<Item = E>,
    {
        self.into_iter().fold(Ok(Ts::default()), |mut res, item| {
            match (&mut res, item) {
                (Ok(ts), Ok(t)) => ts.extend_one(t),
                (Ok(_), Err(e)) => {
                    let mut errs = Esf::default();
                    errs.extend(e);
                    res = Err(errs)
                }
                (Err(_), Ok(_)) => (),
                (Err(es), Err(e)) => es.extend(e),
            }
            res
        })
    }
}
impl<T> CollectOksOrFlatErrs for T {}
