//
//  MWAnimator.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 10..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWAnimator
{
    private static var appendableAnimations: [String : MWAppendableAnimation] = [String : MWAppendableAnimation]()
    
    static func appendableAnimation(_ invokingClass: Any, line: String = String(#line), withDuration duration: TimeInterval, animations: @escaping () -> (), completion: ((Bool) -> ())? = nil) -> String
    {
        return appendableAnimation(invokingClass, withDuration: duration, delay: 0.0, options: .curveLinear, animations: animations, completion: completion)
    }
    
    static func appendableAnimation(identifier: String, withDuration duration: TimeInterval, animations: @escaping () -> (), completion: ((Bool) -> ())? = nil)
    {
        appendableAnimation(identifier: identifier, withDuration: duration, delay: 0.0, options: .curveLinear, animations: animations, completion: completion)
    }
    
    static func appendableAnimation(_ invokingClass: Any, withDuration duration: TimeInterval, delay: TimeInterval, options: UIViewAnimationOptions, animations: @escaping () -> (), completion: ((Bool) -> ())?) -> String
    {
        let identifier: String = generateRandomID(invokingClass)
        appendableAnimation(identifier: identifier, withDuration: duration, delay: delay, options: options, animations: animations, completion: completion)
        return identifier
    }
    
    static func appendableAnimation(identifier: String, withDuration duration: TimeInterval, delay: TimeInterval, options: UIViewAnimationOptions, animations: @escaping () -> (), completion: ((Bool) -> ())?)
    {
        let animation: MWAppendableAnimation = MWAppendableAnimation(completion: completion)
        appendableAnimations[identifier] = animation
        
        UIView.animate(withDuration: duration, delay: delay, options: options, animations: {
            animation.animationInProgress = true
            animations()
        }) { (finished: Bool) in
            finishAnimation(for: identifier, finished: finished)
        }
    }
    
    static func appendableAnimation(_ invokingClass: Any, withDuration duration: TimeInterval, delay: TimeInterval, usingSpringWithDamping dampingRatio: CGFloat, initialSpringVelocity velocity: CGFloat, options: UIViewAnimationOptions, animations: @escaping () -> (), completion: ((Bool) -> ())?) -> String
    {
        let identifier: String = generateRandomID(invokingClass)
        appendableAnimation(identifier: identifier, withDuration: duration, delay: delay, usingSpringWithDamping: dampingRatio, initialSpringVelocity: velocity, options: options, animations: animations, completion: completion)
        return identifier
    }
    
    static func appendableAnimation(identifier: String, withDuration duration: TimeInterval, delay: TimeInterval, usingSpringWithDamping dampingRatio: CGFloat, initialSpringVelocity velocity: CGFloat, options: UIViewAnimationOptions, animations: @escaping () -> (), completion: ((Bool) -> ())?)
    {
        let animation: MWAppendableAnimation = MWAppendableAnimation(completion: completion)
        appendableAnimations[identifier] = animation
        
        UIView.animate(withDuration: duration, delay: delay, usingSpringWithDamping: dampingRatio, initialSpringVelocity: velocity, options: options, animations: {
            animation.animationInProgress = true
            
            animations()
        }) { (finished: Bool) in
            finishAnimation(for: identifier, finished: finished)
        }
    }
    
    static func appendCompletion(to identifier: String, completion: @escaping (Bool) -> ())
    {
        if let animation: MWAppendableAnimation = appendableAnimations[identifier]
        {
            if(animation.animationInProgress)
            {
                animation.appendToCompletionBuffer(completion)
            }
            else
            {
                MWLError("Could not append completion to appendable animation with identifier: \"\(identifier)\", because the animation is no longer in progress.")
            }
        }
        else
        {
            MWLError("There is no appendable animation with identifier: \"\(identifier)\".")
        }
    }
    
    static func animationCurrentlyInProgress(_ identifier: String) -> Bool
    {
        return appendableAnimations[identifier]?.animationInProgress ?? false
    }
    
    private static func generateRandomID(_ invokingClass: Any) -> String
    {
        let ret: String = "MWAnimation\(invokingClass.self)"
        var random: String = ""
        
        while true
        {
            random = String(arc4random_uniform(UInt32(999)))
            
            if(appendableAnimations[ret + random] == nil)
            {
                break
            }
        }
        
        return ret + random
    }
    
    private static func finishAnimation(for identifier: String, finished: Bool)
    {
        let animation: MWAppendableAnimation = appendableAnimations[identifier]!
        
        animation.animationInProgress = false
        appendableAnimations.removeValue(forKey: identifier)
        
        animation.performCompletionBuffer(finished)
    }
}

class MWAppendableAnimation
{
    var animationInProgress: Bool
    private var completionBuffer: [(Bool) -> ()]
    
    init(completion: ((Bool) -> ())? = nil)
    {
        self.animationInProgress = false
        self.completionBuffer = [(Bool) -> ()]()
        
        completion ?! {
            completionBuffer.append(completion!)
        }
    }
    
    func performCompletionBuffer(_ finished: Bool)
    {
        for completion in completionBuffer
        {
            completion(finished)
        }
    }
    
    func appendToCompletionBuffer(_ completion: @escaping (Bool) -> ())
    {
        completionBuffer.append(completion)
    }
}
