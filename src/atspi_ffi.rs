use glib::object::GObject;
use glib_sys::{GDestroyNotify, GError};
use glib_sys::{GHashTable, GPtrArray};
use gobject_sys::{GTypeInterface, GValue};
use gtypes::primitive::{gboolean, gchar, gint, guint};

//=========== Foreign Functions and types
// These were generated using 'bindgen'.

// == types:

type AtspiObject = _AtspiObject;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiObject {
    parent: GObject,
    app: *const ::std::os::raw::c_void,
    path: *mut ::std::os::raw::c_char,
}

pub type AtspiAccessible = _AtspiAccessible;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiAccessible {
    parent: AtspiObject,
    accessible_parent: *mut AtspiAccessible,
    children: *mut GPtrArray,
    role: AtspiRole,
    interfaces: gint,
    pub name: *mut ::std::os::raw::c_char,
    description: *mut ::std::os::raw::c_char,
    states: *const ::std::os::raw::c_void, //AtspiStateSet
    attributes: *mut GHashTable,
    cached_properties: guint,
    priv_: *const ::std::os::raw::c_void, // AtspiAccessiblePrivate
}
pub type AtspiEventListenerCB = ::std::option::Option<
    unsafe extern "C" fn(event: *mut AtspiEvent, user_data: *mut ::std::os::raw::c_void),
>;
type AtspiRole = u32;

pub type AtspiEventListenerMode = _AtspiEventListenerMode;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiEventListenerMode {
    pub synchronous: gboolean,
    pub preemptive: gboolean,
    pub global: gboolean,
}

pub type AtspiEventListener = _AtspiEventListener;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiEventListener {
    parent: GObject,
    callback: AtspiEventListenerCB,
    user_data: *mut ::std::os::raw::c_void,
    cb_destroyed: GDestroyNotify,
}
type AtspiLocaleType = u32;
type AtspiCoordType = u32;
pub const ATSPI_COORD_TYPE_SCREEN: AtspiCoordType = 0;
pub const ATSPI_COORD_TYPE_WINDOW: AtspiCoordType = 1;

pub type AtspiEvent = _AtspiEvent;
#[repr(C)]
#[derive(Copy, Clone)]
pub struct _AtspiEvent {
    pub type_: *mut gchar,
    pub source: *mut AtspiAccessible,
    pub detail1: gint, // Registered events mask (?)
    pub detail2: gint,
    pub any_data: GValue,
}
pub type AtspiText = _AtspiText;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiText {
    parent: GTypeInterface,
}
pub type AtspiRect = _AtspiRect;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiRect {
    pub x: gint,
    pub y: gint,
    pub width: gint,
    pub height: gint,
}
type guint32 = ::std::os::raw::c_uint;
type GQuark = guint32;
type gpointer = *mut ::std::os::raw::c_void;

//
// atspi functions
//

#[link(name = "atspi")]
extern "C" {
    //--------- atspi_misc
    pub fn atspi_init() -> ::std::os::raw::c_int;
    pub fn atspi_exit() -> ::std::os::raw::c_int;
    pub fn atspi_event_main();

    //-------- atspi_text
    pub fn atspi_text_get_caret_offset(obj: *mut AtspiText, error: *mut *mut GError) -> gint;
    pub fn atspi_text_get_character_extents(
        obj: *mut AtspiText,
        offset: gint,
        type_: AtspiCoordType,
        error: *mut *mut GError,
    ) -> *mut AtspiRect;

    //------- atspi_accessible
    pub fn atspi_accessible_get_text_iface(obj: *mut AtspiAccessible) -> *mut AtspiText;

    pub fn atspi_accessible_get_id(obj: *mut AtspiAccessible, error: *mut *mut GError) -> gint;

    pub fn atspi_accessible_get_application(
        obj: *mut AtspiAccessible,
        error: *mut *mut GError,
    ) -> *mut AtspiAccessible;

    //------- atsi_event

    pub fn atspi_event_listener_new(
        callback: AtspiEventListenerCB,
        user_data: gpointer,
        callback_destroyed: GDestroyNotify,
    ) -> *mut AtspiEventListener;

    pub fn atspi_event_listener_register(
        listener: *mut AtspiEventListener,
        event_type: *const gchar,
        error: *mut *mut GError,
    ) -> gboolean;
} // extern "C"
