//
//  MWUtil.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 28..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// Holds all kinds of functions which make creating the app easier.
class MWUtil
{
    //MARK: Static functions
    
    /// Downcasts an object into the specified type.
    ///
    /// - Parameters:
    ///   - to: The parameter, with the specific type, that the new value should be written into.
    ///   - from: The parameter we should downcast from.
    static func downcast<ObjectType>(to: inout ObjectType, from: Any)
    {
        //Check if downcast is possible
        if(from is ObjectType)
        {
            //If yes, downcast
            to = from as! ObjectType
        }
        else
        {
            //If no, throw a fatal error
            fatalError("Could not downcast because the object: \(type(of: from)) is not an instance of: \(ObjectType.self)")
        }
    }
    
    /// Downcasts an object into the specified type and returns the result.
    ///
    /// - Parameters:
    ///   - from: The parameter we should downcast from.
    static func downcastReturn<ObjectType>(from: Any) -> ObjectType
    {
        //Declare the return value
        var ret: ObjectType!
        
        //Downcast into the return value
        downcast(to: &ret, from: from)
        
        //Return the new object
        return ret
    }
    
    /// Executes specified actions if `ifNotNil` is not nil.
    ///
    /// - Parameters:
    ///   - ifNotNil: The parameter the nil-check should be performed on.
    ///   - execution: The execution that should happen if the check reported that the parameter is not nil.
    static func execute<ObjectType>(ifNotNil: ObjectType?, execution: @escaping () -> Swift.Void)
    {
        execute(ifNotNil: ifNotNil, execution: execution, elseExecution: nil)
    }
    
    /// Executes specified actions if `ifNotNil` is not nil, or executes `elseExecution` if `ifNotNil` is nil.
    ///
    /// - Parameters:
    ///   - ifNotNil: The parameter the nil-check should be performed on.
    ///   - execution: The execution that should happen if the check reported that the parameter is not nil.
    ///   - elseExecution: The execution that should happen if the check reported that the parameter is nil.
    static func execute<ObjectType>(ifNotNil: ObjectType?, execution: @escaping () -> Swift.Void, elseExecution: (() -> Swift.Void)?)
    {
        //Check if nil
        if(ifNotNil != nil)
        {
            //If not, execute
            execution()
        }
        else
        {
            //It it is, check for executions that should happen this case
            if(elseExecution != nil)
            {
                //If not nil, execute
                elseExecution!()
            }
            else if(myWatch.get().debugMode)
            {
                //If nil, and the application uses debug mode, log an error message
                MWLError("No executions happened because the specified parameter with type: \(ObjectType.self) was nil and there was no else execution specified.", module: .moduleCore)
            }
        }
    }
    
    /// Executes specified actions if `ifNil` is nil.
    ///
    /// - Parameters:
    ///   - ifNil: The parameter the nil-check should be performed on.
    ///   - execution: The execution that should happen if the check reported that the parameter is nil.
    static func execute<ObjectType>(ifNil: ObjectType?, execution: @escaping () -> Swift.Void)
    {
        execute(ifNil: ifNil, execution: execution, elseExecution: nil)
        
        
    }
    
    /// Executes specified actions if `ifNil` is nil, or executes `elseExecution` if `ifNil` is not nil.
    ///
    /// - Parameters:
    ///   - ifNil: The parameter the nil-check should be performed on.
    ///   - execution: The execution that should happen if the check reported that the parameter is nil.
    ///   - elseExecution: The execution that should happen if the check reported that the parameter is not nil.
    static func execute<ObjectType>(ifNil: ObjectType?, execution: @escaping () -> Swift.Void, elseExecution: (() -> Swift.Void)?)
    {
        //Check if nil
        if(ifNil == nil)
        {
            //If it is, execute
            execution()
        }
        else
        {
            //It not, check for executions that should happen this case
            if(elseExecution != nil)
            {
                //If not nil, execute
                elseExecution!()
            }
            else if(myWatch.get().debugMode)
            {
                //If nil, and the application uses debug mode, log an error message
                MWLError("No executions happened because the specified parameter with type: \(ObjectType.self) was not nil and there was no else execution specified.", module: .moduleCore)
            }
        }
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //                                                                                                                    //
    //  Soon, this function will replace "execute(ifNotNil:execution:)" and "execute(ifNotNil:execution:elseExecution)".  //
    //                                                                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    /// Checks whether the specified object is not nil, and executes blocks based on the result.
    ///
    /// - Parameters:
    ///   - object: The object the check should be performed on.
    ///   - not: The execution that should happen if the object was not nil.
    ///   - _nil: The execution that should happen if the object was nil.
    static func nilcheck<ObjectType>(_ object: ObjectType?, not: (() -> Swift.Void), nil _nil: (() -> Swift.Void)? = nil)
    {
        //Check if object is not nil
        if(object != nil)
        {
            //If not nil, execute
            not()
        }
        else
        {
            //If nil, check for executions that should happen this case
            if(_nil != nil)
            {
                //If not nil, execute
                _nil!()
            }
            else if(myWatch.get().debugMode)
            {
                //If nil, and the application uses debug mode, log an error message
                MWLError("No executions happened because the specified parameter with type: \(ObjectType.self) was nil and there was no execution specified for this case.", module: .moduleCore)
            }
        }
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //                                                                                                              //
    //  Soon, this function will replace "execute(ifNil:execution:)" and "execute(ifNil:execution:elseExecution)".  //
    //                                                                                                              //
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    /// Checks whether the specified object is nil, and executes blocks based on the result.
    ///
    /// - Parameters:
    ///   - object: The object the check should be performed on.
    ///   - _nil: The execution that should happen if the object was nil.
    ///   - not: The execution that should happen if the object was not nil.
    static func nilcheck<ObjectType>(_ object: ObjectType?, nil _nil: (() -> Swift.Void), not: (() -> Swift.Void)? = nil)
    {
        //Check if object is nil
        if(object == nil)
        {
            //If nil, execute
            _nil()
        }
        else
        {
            //It not, check for executions that should happen this case
            if(not != nil)
            {
                //If not nil, execute
                not!()
            }
            else if(myWatch.get().debugMode)
            {
                //If nil, and the application uses debug mode, log an error message
                MWLError("No executions happened because the specified parameter with type: \(ObjectType.self) was not nil and there was no execution specified for this case.", module: .moduleCore)
            }
        }
    }
}

//MARK: -

/// Used to attach an animation defined by this to an image view.
class MWImageAnimation
{
    //MARK: Instance variables
    
    /// The number of times this animation should be repeated.
    ///
    /// - If `0`, the animation will play in a loop.
    var repeatCount: Int
    
    /// The duration of the animation.
    var duration: TimeInterval
    
    /// Every frame of this animation.
    var frames: [UIImage]
    
    //MARK: - Initializers
    
    /// Makes an `MWImageAnimation` instance out of the given parameters.
    ///
    /// - Parameters:
    ///   - repeatCount: The number of times this animation should be repeated.
    ///   - duration: The duration of the animation.
    ///   - frames: Every frame of this animation.
    init(repeatCount: Int, duration: TimeInterval, frames: [UIImage])
    {
        self.repeatCount = repeatCount
        self.duration = duration
        self.frames = frames
    }
}

//MARK: -

/// Our own gradient implementation.
class MWGradient
{
    //MARK: Instance variables
    
    /// Holds the colors of this gradient.
    var colors: [UIColor]
    
    /// Holds the colors of this gradient as `CGColor` references.
    var cgColors: [CGColor]
    
    /// Holds the stops where the corresponding colors should be placed.
    var points: [CGFloat]
    
    //MARK: - Initializers
    
    /// Makes an `MWGradient` instance out of the given parameters.
    ///
    /// - Parameters:
    ///   - colors: The colors the gradient should use.
    ///   - points: The points within the gradient where the colors should be placed.
    init(colors: UIColor..., points: [CGFloat]? = nil)
    {
        //Store the parameters
        self.colors = colors
        self.points = [CGFloat]()
        
        self.cgColors = [CGColor]()
        
        //Convert the UIColors into CGColors
        self.colors.forEach { (color: UIColor) in
            cgColors.append(color.cgColor)
        }
        
        //Spread the points if we have more than two colors
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
            //If we only have two, distribute from start to end
            self.points = [0.0, 1.0]
        }
    }
    
    //MARK: Instance functions
    
    /// Converts the `MWGradient` instance into `CGGradient`.
    ///
    /// - Returns: The `CGGradient` constructed from this gradient.
    func cgGradient() -> CGGradient
    {
        //Create the color space
        let colorSpace: CGColorSpace = CGColorSpaceCreateDeviceRGB()
        
        //Make the return value
        let ret: CGGradient = CGGradient(colorsSpace: colorSpace, colors: cgColors as CFArray, locations: points)!
        
        //Return the gradient
        return ret
    }
}

//MARK: -

/// A small class which acts like a "queue" collection.
///
/// Works similarly to a stack, but exactly inversed.
///
/// When we push, as in a stack, we push to the end of the array.
///
/// However when we pop, we do not pop the last item of the array, but the first.
class MWQueue<Type>
{
    //MARK: Instance variables
    
    /// The array which holds the queued items.
    ///
    /// Initialized as an empty array or can be given an inital value in `init(with:)`
    private var queue: [Type]
    
    /// The number of items in the queue.
    ///
    /// Updated whenever we remove or add an item to the queue.
    var count: Int
    
    //MARK: - Initializers
    
    /// Initializer which creates an empty queue.
    ///
    /// - Returns: An initialized `MWQueue` instance.
    init()
    {
        self.queue = [Type]()
        self.count = 0
    }
    
    /// Initializer which creates a queue with the specified items.
    ///
    /// The queue order will be as the order of items in list `items`
    ///
    /// - Parameter items: The items that will be copied over to the queue.
    ///
    /// - Returns: An initialized `MWQueue` instance.
    init(with items: Type...)
    {
        self.queue = items
        self.count = queue.count
    }
    
    /// Initializer which creates a queue with the specified array of items.
    ///
    /// The queue order will be as the order of items in array `items`
    ///
    /// - Parameter items: The items that will be copied over to the queue.
    ///
    /// - Returns: An initialized `MWQueue` instance.
    init(with items: [Type])
    {
        self.queue = items
        self.count = queue.count
    }
    
    //MARK: Instance functions
    
    /// Function which adds (pushes) an item to the end of the queue.
    ///
    /// - Parameter item: The item that will be added to the end of the queue.
    func push(_ item: Type)
    {
        queue.append(item)
        count += 1
    }
    
    /// Function which returns the first item from the queue without removing it from the queue.
    ///
    /// - Returns: The upcoming (first) item in the queue.
    func peek() -> Type
    {
        return queue[0]
    }
    
    /// Function which removes the upcoming (first) item from the queue.
    func pop()
    {
        queue.remove(at: 0)
        count -= 1
    }
    
    /// Function which removes the upcoming (first) item from the queue and than returns it.
    ///
    /// - Returns: The popped (upcoming/first) item of the queue (which is no longer in the queue).
    func popReturn() -> Type
    {
        let ret = queue[0]
        queue.remove(at: 0)
        
        count -= 1
        
        return ret
    }
    
    /// Function which removes an item at the specified location.
    func pop(at: Int)
    {
        queue.remove(at: at)
        count -= 1
    }
    
    /// Function which removes an item at the specified location and than returns it.
    ///
    /// - Returns: The popped item of the queue (which is no longer in the queue).
    func popReturn(at: Int) -> Type
    {
        let ret = queue[at]
        queue.remove(at: at)
        
        count -= 1
        
        return ret
    }
}
