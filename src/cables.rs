use notify_rust::Notification;

pub(crate) fn notify_stopped() {
    Notification::new()
        .summary("tow notification cable")
        .body(" stopped tow ")
        .icon("logviewer")
        .show()
        .expect("Notify stop failed");
}

pub(crate) fn notify_started() {
    Notification::new()
        .summary("tow notification cable")
        .body(" started tow ")
        .icon("logviewer")
        .show()
        .expect("Notify start failed");
}
