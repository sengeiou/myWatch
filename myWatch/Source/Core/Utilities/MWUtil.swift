//
//  MWUtil.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 28..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWUtil
{
    static func safelySetValue<ObjectType>(_ toSet: inout ObjectType?, toValue: ObjectType)
    {
        if(toSet != nil)
        {
            toSet = toValue
        }
        else
        {
            MWLError("Failed to set the value of object specified in parameter.", module: .moduleCore)
        }
    }
    
    static func downcast<ObjectType>(to: inout ObjectType, from: Any)
    {
        if(from is ObjectType)
        {
            to = from as! ObjectType
        }
        else
        {
            fatalError("Could not downcast because the object: \(type(of: from)) is not an instance of: \(ObjectType.self)")
        }
    }
    
    static func downcastReturn<ObjectType>(from: Any) -> ObjectType
    {
        if(from is ObjectType)
        {
            return from as! ObjectType
        }
        else
        {
            fatalError("Could not downcast because the object: \(type(of: from)) is not an instance of: \(ObjectType.self)")
        }
    }
    
    static func execute<ObjectType>(ifNotNil: ObjectType?, execution: @escaping () -> Swift.Void)
    {
        execute(ifNotNil: ifNotNil, execution: execution, elseExecution: nil)
    }
    
    static func execute<ObjectType>(ifNotNil: ObjectType?, execution: @escaping () -> Swift.Void, elseExecution: (() -> Swift.Void)?)
    {
        if(ifNotNil != nil)
        {
            execution()
        }
        else
        {
            if(elseExecution != nil)
            {
                elseExecution!()
            }
            else if(myWatch.get().debugMode)
            {
                MWLError("No executions happened because the specified parameter with type: \(ObjectType.self) was nil and there was no else execution specified.", module: .moduleCore)
            }
        }
    }
    
    static func execute<ObjectType>(ifNil: ObjectType?, execution: @escaping () -> Swift.Void)
    {
        execute(ifNil: ifNil, execution: execution, elseExecution: nil)
    }
    
    static func execute<ObjectType>(ifNil: ObjectType?, execution: @escaping () -> Swift.Void, elseExecution: (() -> Swift.Void)?)
    {
        if(ifNil == nil)
        {
            execution()
        }
        else
        {
            if(elseExecution != nil)
            {
                elseExecution!()
            }
            else if(myWatch.get().debugMode)
            {
                MWLError("No executions happened because the specified parameter with type: \(ObjectType.self) was not nil and there was no else execution specified.", module: .moduleCore)
            }
        }
    }
}

class MWGradient
{
    var colors: [UIColor]
    var cgColors: [CGColor]
    var points: [CGFloat]
    
    init(colors: UIColor..., points: [CGFloat]? = nil)
    {
        self.colors = colors
        self.points = [CGFloat]()
        
        self.cgColors = [CGColor]()
        
        self.colors.forEach { (color: UIColor) in
            cgColors.append(color.cgColor)
        }
        
        if(colors.count > 2)
        {
            MWUtil.execute(ifNil: points, execution: { 
                let spread: CGFloat = CGFloat(1 / colors.count)
                
                for i in 0..<self.colors.count
                {
                    self.points.append(spread * CGFloat(i + 1))
                }
            }, elseExecution: { 
                self.points = points!
            })
        }
        else
        {
            self.points = [0.0, 1.0]
        }
    }
    
    func cgGradient() -> CGGradient
    {
        var _colors: [CGColor] = [CGColor]()
        
        for color in colors
        {
            _colors.append(color.cgColor)
        }
        
        let colorSpace: CGColorSpace = CGColorSpaceCreateDeviceRGB()
        let ret: CGGradient = CGGradient(colorsSpace: colorSpace, colors: _colors as CFArray, locations: points)!
        
        return ret
    }
}
