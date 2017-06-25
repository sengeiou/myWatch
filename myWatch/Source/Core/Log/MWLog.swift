//
//  MWLog.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 12..
//  Copyright © 2017. theMatys. All rights reserved.
//

import os.log

//MARK: Logging functions

/// Used to display informations.
///
/// When we call this function, it downcasts the parameter "toLog" to type CVarArg, because only CVarArg parameters can be logged with the os_log function.
///
/// If we specify a logging module, it displays the name of that before the message we are logging.
///
/// - See: `MWLModule` for more details on the modules below.
func MWLInfo(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@", log: OSLog.default, type: .info, module!.rawValue as CVarArg, _toLog)
    }) { 
        os_log("%@", log: OSLog.default, type: .info, _toLog)
    }
}

/// Used to display errors.
///
/// When we call this function, it downcasts the parameter "toLog" to type CVarArg, because only CVarArg parameters can be logged with the os_log function.
///
/// It writes "ERROR: " before the message (and the module if specified) to indicate that the message reports an error.
///
/// If we specify a logging module, it displays the name of that before the message we are logging.
///
/// - See: `MWLModule` for more details on the modules below.
func MWLError(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@%@", log: OSLog.default, type: .error, "ERROR: ", module!.rawValue as CVarArg, _toLog)
    }) {
        os_log("%@%@", log: OSLog.default, type: .error, "ERROR: ", _toLog)
    }
}

/// Used to display fatal errors.
///
/// When we call this function, it downcasts the parameter "toLog" to type CVarArg, because only CVarArg parameters can be logged with the os_log function.

/// It writes "FAULT: " before the message (and the module if specified) to indicate that the message reports a fatal error.
///
///If we specify a logging module, it displays the name of that before the message we are logging.
///
/// - See: `MWLModule` for more details on the modules below.
func MWLFault(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@%@", log: OSLog.default, type: .fault, "FAULT: ", module!.rawValue as CVarArg, _toLog)
    }) {
        os_log("%@%@", log: OSLog.default, type: .fault, "FAULT: ", _toLog)
    }
}

/// Used to display debug messages.
///
/// When we call this function, it downcasts the parameter "toLog" to type CVarArg, because only CVarArg parameters can be logged with the os_log function.
///
/// It writes "DEBUG: " before the message (and the module if specified) to indicate that the message reports a debug message.
/// 
/// If we specify a logging module, it displays the name of that before the message we are logging.
/// 
/// - See: `MWLModule` for more details on the modules below.
func MWLDebug(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@%@", log: OSLog.default, type: .debug, "DEBUG: ", module!.rawValue as CVarArg, _toLog)
    }) {
        os_log("%@%@", log: OSLog.default, type: .debug, "DEBUG: ", _toLog)
    }
}

//MARK: -

/// Enum for different prefixes based on the part of the application we're logging from.
///
/// Its purpose is only to make the log more organized and understandable.
enum MWLModule : String
{
    case moduleCore = "[CORE]: "
    case moduleBluetooth = "[BLUETOOTH]: "
    case moduleIO = "[IO]: "
}
