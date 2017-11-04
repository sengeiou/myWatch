//
//  MWDispatcher.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 27..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

class MWDispatcher
{
    private static var appendableAsynchronizations: [String : MWAppendableAsynchronization] = [String : MWAppendableAsynchronization]()
    
    static func appendableAsyncAfter(_ invokingClass: Any, deadline: TimeInterval, execution: (() -> ())?) -> String
    {
        let identifier: String = generateRandomID(invokingClass)
        appendableAsyncAfter(identifier: identifier, deadline: deadline, execution: execution)
        return identifier
    }
    
    static func appendableAsyncAfter(identifier: String, deadline: TimeInterval, execution: (() -> ())?)
    {
        let appendableAsynchronization: MWAppendableAsynchronization = MWAppendableAsynchronization(execution: execution)
        appendableAsynchronizations[identifier] = appendableAsynchronization
        
        appendableAsynchronization.inProgress = true
        
        DispatchQueue.main.asyncAfter(deadline: .now() + deadline) { 
            async(identifier)
        }
    }
    
    static func asynchronizationCurrentlyInProgress(_ identifier: String) -> Bool
    {
        return appendableAsynchronizations[identifier]?.inProgress ?? false
    }
    
    static func appendExecution(to identifier: String, execution: @escaping () -> ())
    {
        if let appendableAsynchronization: MWAppendableAsynchronization = appendableAsynchronizations[identifier]
        {
            if(appendableAsynchronization.inProgress)
            {
                appendableAsynchronization.appendExecution(execution)
            }
            else
            {
                MWLError("Could not append execution to appendable asnychronization with identifier: \"\(identifier)\", because the asynchronization is no longer in progress.")
            }
        }
        else
        {
            MWLError("There is no appendable asnychronization with identifier: \"\(identifier)\".")
        }
    }
    
    private static func generateRandomID(_ invokingClass: Any) -> String
    {
        let ret: String = "MW\(invokingClass.self)"
        var random: String = ""
        
        while true
        {
            random = String(arc4random_uniform(UInt32(999)))
            
            if(appendableAsynchronizations[ret + random] == nil)
            {
                break
            }
        }
        
        return ret + random + "AppendableAsynchronizationIdentifier"
    }
    
    private static func async(_ identifier: String)
    {
        let appendableAsynchronization: MWAppendableAsynchronization = appendableAsynchronizations[identifier]!
        
        appendableAsynchronization.inProgress = false
        appendableAsynchronizations.removeValue(forKey: identifier)
        
        appendableAsynchronization.performExecutions()
    }
}

class MWAppendableAsynchronization
{
    var inProgress: Bool
    private var executionBuffer: [() -> ()]
    
    init(execution: (() -> ())?)
    {
        self.inProgress = false
        self.executionBuffer = [() -> ()]()
        
        execution ?! {
            executionBuffer.append(execution!)
        }
    }
    
    func performExecutions()
    {
        for execution in executionBuffer
        {
            execution()
        }
    }
    
    func appendExecution(_ execution: @escaping () -> ())
    {
        executionBuffer.append(execution)
    }
}
