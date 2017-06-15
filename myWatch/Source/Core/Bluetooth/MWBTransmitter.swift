//
//  MWBTransmitter.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 17..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

class MWBTransmitter
{
    //MARK: Commands - Unknown commands
    private static let COMMAND_LLS0: MWBCommand = MWBCommand(command: "$LLS0", nonResponse: true)
    private static let COMMAND_Z: MWBCommand = MWBCommand(command: "$Z", nonResponse: true)
    private static let COMMAND_BFS: MWBCommand = MWBCommand(command: "$BFS", nonResponse: true)
    private static let COMMAND_BFE: MWBCommand = MWBCommand(command: "$BFE", nonResponse: true)
    
    //MARK: Commands - Retriever commands
    private static let COMMAND_RETRIEVE_VERSION: MWBCommand = MWBCommand(command: "$RV")
    private static let COMMAND_RETRIEVE_CURRENT_STEP_COUNT: MWBCommand = MWBCommand(command: "S")
    private static let COMMAND_RETRIEVE_SERIAL_NUMBER: MWBCommand = MWBCommand(command: "$RSN")
    
    //MARK: Commands - Functionality commands
    private static let COMMAND_ANSWER_PHONE_CALL: MWBCommand = MWBCommand(command: "$PA", nonResponse: true)
    private static let COMMAND_DECLINE_PHONE_CALL: MWBCommand = MWBCommand(command: "$PD", nonResponse: true)
    private static let COMMAND_ENTER_CAMERA_MODE: MWBCommand = MWBCommand(command: "$CSS", nonResponse: true)
    private static let COMMAND_EXIT_CAMERA_MODE: MWBCommand = MWBCommand(command: "$CSE", nonResponse: true)
    
    //MARK: - Static functions
    static func setAttributeTimeToCurrentTime(communicator: MWBCommunicator)
    {
        let date: Date = Date()
        let calendar: Calendar = Calendar.current
        
        let year: Int   = calendar.component(.year, from: date) - 2000
        let month: Int  = calendar.component(.month, from: date)
        let day: Int    = calendar.component(.day, from: date)
        let hour: Int   = calendar.component(.hour, from: date)
        let minute: Int = calendar.component(.minute, from: date)
        let second: Int = calendar.component(.second, from: date)
        
        let year_str: String = String(year)
        let month_str: String = String(month)
        let day_str: String = String(day)
        let hour_str: String = String(hour)
        let minute_str: String = String(minute)
        let second_str: String = String(second)
        
        var time: String = year_str
        time.append(month_str.characters.count == 1 ? "0" + month_str : month_str)
        time.append(day_str.characters.count == 1 ? "0" + day_str : day_str)
        time.append(hour_str.characters.count == 1 ? "0" + hour_str : hour_str)
        time.append(minute_str.characters.count == 1 ? "0" + minute_str : minute_str)
        time.append(second_str.characters.count == 1 ? "0" + second_str : second_str)
        
        self.setAttributeTime(communicator: communicator, time: time)
    }
    
    static func setAttributeTime(communicator: MWBCommunicator, time: String)
    {
        var stringCommand: String = "$T"
        stringCommand.append(time)
        
        let command: MWBCommand = MWBCommand(command: stringCommand, nonResponse: true)
        communicator.sendCommand(command: command)
    }
    
    static func setAttributeLLS0(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_LLS0)
    }
    
    static func setAttributeZ(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_Z)
    }
    
    static func retrieveCurrentVersion(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_RETRIEVE_VERSION)
    }
    
    static func retrieveCurrentStepCount(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_RETRIEVE_CURRENT_STEP_COUNT)
    }
    
    static func retrieveSerialNumber(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_RETRIEVE_SERIAL_NUMBER)
    }
    
    static func enterCameraMode(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_ENTER_CAMERA_MODE)
    }
    
    static func exitCameraMode(communicator: MWBCommunicator)
    {
        communicator.sendCommand(command: COMMAND_EXIT_CAMERA_MODE)
    }
}
