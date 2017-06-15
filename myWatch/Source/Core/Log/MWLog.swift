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
    if(module != nil)
    {
        os_log("%@%@", log: OSLog.default, type: .info, module!.rawValue as CVarArg, toLog as! CVarArg)
    }
    else
    {
        os_log("%@", log: OSLog.default, type: .info, toLog as! CVarArg)
    }
}

func MWLError(_ toLog: Any?, module: MWLModule?)
{
    if(module != nil)
    {
        os_log("%@%@%@", log: OSLog.default, type: .error, module!.rawValue as CVarArg, "ERROR: ", toLog as! CVarArg)
    }
    else
    {
        os_log("%@%@", log: OSLog.default, type: .error, "ERROR: ", toLog as! CVarArg)
    }
}

func MWLFault(_ toLog: Any?, module: MWLModule?)
{
    if(module != nil)
    {
        os_log("%@%@%@", log: OSLog.default, type: .fault, module!.rawValue as CVarArg, "FAULT: ", toLog as! CVarArg)
    }
    else
    {
        os_log("%@%@", log: OSLog.default, type: .fault, "FAULT: ", toLog as! CVarArg)
    }
}

func MWLDebug(_ toLog: Any?, module: MWLModule?)
{
    if(module != nil)
    {
        os_log("%@%@%@", log: OSLog.default, type: .debug, module!.rawValue as CVarArg, "DEBUG: ", toLog as! CVarArg)
    }
    else if(toLog != nil)
    {
        os_log("%@%@", log: OSLog.default, type: .debug, "DEBUG: ", toLog as! CVarArg)
    }
    else
    {
        os_log("%@%@", log: OSLog.default, type: .debug, "DEBUG: ", "nil")
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
