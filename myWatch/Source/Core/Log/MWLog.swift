//
//  MWLog.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 12..
//  Copyright © 2017. theMatys. All rights reserved.
//

import os.log

//MARK: Logging functions
func MWLInfo(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@", log: OSLog.default, type: .info, module!.rawValue as CVarArg, _toLog)
    }) { 
        os_log("%@", log: OSLog.default, type: .info, _toLog)
    }
}

func MWLError(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@%@", log: OSLog.default, type: .error, "ERROR: ", module!.rawValue as CVarArg, _toLog)
    }) {
        os_log("%@%@", log: OSLog.default, type: .error, "ERROR: ", _toLog)
    }
}

func MWLFault(_ toLog: Any?, module: MWLModule?)
{
    let _toLog: CVarArg = MWUtil.downcastReturn(from: toLog ?? "nil")
    
    MWUtil.execute(ifNotNil: module, execution: {
        os_log("%@%@%@", log: OSLog.default, type: .fault, "FAULT: ", module!.rawValue as CVarArg, _toLog)
    }) {
        os_log("%@%@", log: OSLog.default, type: .fault, "FAULT: ", _toLog)
    }
}

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

//Enum for different prefixes based on the part of the application we're logging from.
//Its purpose is only to make the log more organized and understandable.
enum MWLModule : String
{
    case moduleCore = "[CORE]: "
    case moduleBluetooth = "[BLUETOOTH]: "
    case moduleIO = "[IO]: "
}
