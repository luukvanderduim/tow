#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

use glib::object::GObject;
use glib_sys::{GArray, GDestroyNotify, GError};
use glib_sys::{GHashTable, GPtrArray};
use gobject_sys::{GTypeInterface, GValue};
use gtypes::primitive::{gboolean, gchar, gint, guint};

//=========== Foreign Functions and types
// These were generated using 'bindgen'.

// == types:

pub type AtspiObject = _AtspiObject;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiObject {
    parent: GObject,
    app: *const ::std::os::raw::c_void,
    path: *mut ::std::os::raw::c_char,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiAccessiblePrivate {
    _unused: [u8; 0],
}

pub type AtspiAccessiblePrivate = _AtspiAccessiblePrivate;

pub type AtspiAccessible = _AtspiAccessible;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiAccessible {
    pub parent: AtspiObject,
    pub accessible_parent: *mut AtspiAccessible,
    pub children: *mut GPtrArray,
    pub role: AtspiRole,
    pub interfaces: gint,
    pub name: *mut ::std::os::raw::c_char,
    pub description: *mut ::std::os::raw::c_char,
    pub states: *mut AtspiStateSet,
    pub attributes: *mut GHashTable,
    pub cached_properties: guint,
    pub priv_: *mut AtspiAccessiblePrivate,
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

pub type AtspiEditableText = _AtspiEditableText;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiEditableText {
    pub parent: GTypeInterface,
}

pub type AtspiStateSet = _AtspiStateSet;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _AtspiStateSet {
    pub parent: GObject,
    pub accessible: *mut _AtspiAccessible,
    pub states: gint64,
}

pub type gint64 = ::std::os::raw::c_long;

pub type AtspiStateType = u32;

pub type AtspiTextClipType = u32;
pub const AtspiStateType_ATSPI_STATE_INVALID: AtspiStateType = 0;
pub const AtspiStateType_ATSPI_STATE_ACTIVE: AtspiStateType = 1;
pub const AtspiStateType_ATSPI_STATE_ARMED: AtspiStateType = 2;
pub const AtspiStateType_ATSPI_STATE_BUSY: AtspiStateType = 3;
pub const AtspiStateType_ATSPI_STATE_CHECKED: AtspiStateType = 4;
pub const AtspiStateType_ATSPI_STATE_COLLAPSED: AtspiStateType = 5;
pub const AtspiStateType_ATSPI_STATE_DEFUNCT: AtspiStateType = 6;
pub const AtspiStateType_ATSPI_STATE_EDITABLE: AtspiStateType = 7;
pub const AtspiStateType_ATSPI_STATE_ENABLED: AtspiStateType = 8;
pub const AtspiStateType_ATSPI_STATE_EXPANDABLE: AtspiStateType = 9;
pub const AtspiStateType_ATSPI_STATE_EXPANDED: AtspiStateType = 10;
pub const AtspiStateType_ATSPI_STATE_FOCUSABLE: AtspiStateType = 11;
pub const AtspiStateType_ATSPI_STATE_FOCUSED: AtspiStateType = 12;
pub const AtspiStateType_ATSPI_STATE_HAS_TOOLTIP: AtspiStateType = 13;
pub const AtspiStateType_ATSPI_STATE_HORIZONTAL: AtspiStateType = 14;
pub const AtspiStateType_ATSPI_STATE_ICONIFIED: AtspiStateType = 15;
pub const AtspiStateType_ATSPI_STATE_MODAL: AtspiStateType = 16;
pub const AtspiStateType_ATSPI_STATE_MULTI_LINE: AtspiStateType = 17;
pub const AtspiStateType_ATSPI_STATE_MULTISELECTABLE: AtspiStateType = 18;
pub const AtspiStateType_ATSPI_STATE_OPAQUE: AtspiStateType = 19;
pub const AtspiStateType_ATSPI_STATE_PRESSED: AtspiStateType = 20;
pub const AtspiStateType_ATSPI_STATE_RESIZABLE: AtspiStateType = 21;
pub const AtspiStateType_ATSPI_STATE_SELECTABLE: AtspiStateType = 22;
pub const AtspiStateType_ATSPI_STATE_SELECTED: AtspiStateType = 23;
pub const AtspiStateType_ATSPI_STATE_SENSITIVE: AtspiStateType = 24;
pub const AtspiStateType_ATSPI_STATE_SHOWING: AtspiStateType = 25;
pub const AtspiStateType_ATSPI_STATE_SINGLE_LINE: AtspiStateType = 26;
pub const AtspiStateType_ATSPI_STATE_STALE: AtspiStateType = 27;
pub const AtspiStateType_ATSPI_STATE_TRANSIENT: AtspiStateType = 28;
pub const AtspiStateType_ATSPI_STATE_VERTICAL: AtspiStateType = 29;
pub const AtspiStateType_ATSPI_STATE_VISIBLE: AtspiStateType = 30;
pub const AtspiStateType_ATSPI_STATE_MANAGES_DESCENDANTS: AtspiStateType = 31;
pub const AtspiStateType_ATSPI_STATE_INDETERMINATE: AtspiStateType = 32;
pub const AtspiStateType_ATSPI_STATE_REQUIRED: AtspiStateType = 33;
pub const AtspiStateType_ATSPI_STATE_TRUNCATED: AtspiStateType = 34;
pub const AtspiStateType_ATSPI_STATE_ANIMATED: AtspiStateType = 35;
pub const AtspiStateType_ATSPI_STATE_INVALID_ENTRY: AtspiStateType = 36;
pub const AtspiStateType_ATSPI_STATE_SUPPORTS_AUTOCOMPLETION: AtspiStateType = 37;
pub const AtspiStateType_ATSPI_STATE_SELECTABLE_TEXT: AtspiStateType = 38;
pub const AtspiStateType_ATSPI_STATE_IS_DEFAULT: AtspiStateType = 39;
pub const AtspiStateType_ATSPI_STATE_VISITED: AtspiStateType = 40;
pub const AtspiStateType_ATSPI_STATE_CHECKABLE: AtspiStateType = 41;
pub const AtspiStateType_ATSPI_STATE_HAS_POPUP: AtspiStateType = 42;
pub const AtspiStateType_ATSPI_STATE_READ_ONLY: AtspiStateType = 43;
pub const AtspiStateType_ATSPI_STATE_LAST_DEFINED: AtspiStateType = 44;

//
// atspi functions
//

#[link(name = "atspi")]
extern "C" {
    //--------- atspi state

    pub fn atspi_state_set_contains(set: *mut AtspiStateSet, state: AtspiStateType) -> gboolean;
    pub fn atspi_state_set_get_states(set: *mut AtspiStateSet) -> *mut GArray;

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

    pub fn atspi_accessible_get_editable_text(obj: *mut AtspiAccessible) -> *mut AtspiEditableText;

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
