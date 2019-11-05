#[cfg(test)]
#[test]
fn test_get_sigmoid_shape() {
    use super::get_sigmoid_shape;

    let n: usize = get_sigmoid_shape().len();
    let mut shape: Vec<f64> = get_sigmoid_shape();
    for _ in 0..(n / 2) {
        assert_eq!(shape.first(), shape.last());
        shape.pop();
        shape.remove(0);
    }
}

#[cfg(test)]
#[test]
fn test_do_tow() {
    use super::do_tow;
    use crate::point::Point;
    use rand::{thread_rng, Rng};
    use std::sync::Arc;

    let mut rng = thread_rng();

    let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");
    let conn = Arc::new(conn);
    for _ in 0..10 {
        let p: Point = Point(rng.gen_range(-1001, 1001), rng.gen_range(-1001, 1001));
        let q: Point = Point(rng.gen_range(0, 1921), rng.gen_range(0, 1080));
        let ans = p + q;
        let r: Point = do_tow(p, q.0, q.1, conn.clone(), screen_num);
        assert_eq!(r, ans);
    }
}
