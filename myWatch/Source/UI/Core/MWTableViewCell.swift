//
//  MWTableViewCell.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 15..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWTableViewCell: UITableViewCell
{
    @IBInspectable var mode: Int = 1
    {
        didSet
        {
            _mode = MWTableViewCellMode(rawValue: MWUtil.clamp(mode - 1, min: 0, max: MWTableViewCellMode.count))!
        }
    }
    
    @IBInspectable var style: Int = 1
    {
        didSet
        {
            _style = MWTableViewCellStyle(rawValue: MWUtil.clamp(style - 1, min: 0, max: MWTableViewCellStyle.count))!
        }
    }
    
    @IBInspectable var animateAll: Bool = false
    
    override var isHighlighted: Bool
    {
        didSet
        {
            setHighlighted(isHighlighted, animated: true)
        }
    }
    
    override var isSelected: Bool
    {
        didSet
        {
            setSelected(isSelected, animated: true)
        }
    }
    
    override var backgroundColor: UIColor?
    {
        didSet
        {
            if(!silent)
            {
                _init()
            }
            else
            {
                silent = false
            }
        }
    }
    
    internal var _mode: MWTableViewCellMode = .normal
    {
        didSet
        {
            _init()
        }
    }
    
    internal var _style: MWTableViewCellStyle = .dark
    {
        didSet
        {
            _init()
        }
    }
    
    var isToggled: Bool = false
    {
        didSet
        {
            setToggled(isToggled, animated: true)
        }
    }
        
    internal var silent: Bool = false
    
    private var selectedBackgroundColor: UIColor?
    private var unselectedBackgroundColor: UIColor?
    
    private let highlightAnimationIdentifier: String = "MWAnimationMWTableViewCellHighlight"
    private let animationDuration: TimeInterval = 0.1
    
    private var originalAccessory: UITableViewCellAccessoryType!
    
    /// A boolean which indicates whether the cell should do the unhighlight animation automatically after the highlight animation.
    ///
    /// When `true`, the unhighlight animation is done automatically after the highlight animation.
    ///
    /// When `false`, the unhighlight animation is done when the user unhighlights the cell.
    ///
    /// The value stays `true` when the cell is set to toggle mode, and when the user tapped the cell and did not hold it. In this case, the execution of the unhighlight animation would interrupt the highlight animation in such an early stage that no animations would be seen. Therefore, the unhighlight animation is executed after the highlight animation.
    ///
    /// However if the user taps and holds the cell until the highlight animation finishes, its value is set to `false` to indicate that the unhighlight animation can be executed when the user unhighlights the cell, because it would not interrupt anything.
    private var automaticUnhighlight: Bool = true
    
    override init(style: UITableViewCellStyle, reuseIdentifier: String?)
    {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        
        _init()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        _init()
    }
    
    override func setHighlighted(_ highlighted: Bool, animated: Bool)
    {
        super.setHighlighted(highlighted, animated: animated)
        
        //Check if the cell is set to silent mode
        if(!silent)
        {
            //Check if the cell is set to toggle mode
            if(_mode == .toggle)
            {
                //Check if the cell is about to be highlighted
                if(highlighted)
                {
                    //Check if the operation was set to animate
                    if(animated || animateAll)
                    {
                        UIView.animate(withDuration: animationDuration, delay: 0.0, options: .curveEaseOut, animations: {
                            self.silently().backgroundColor = self.selectedBackgroundColor
                        }, completion: { (finished: Bool) in
                            if(!self.isHighlighted)
                            {
                                UIView.animate(withDuration: self.animationDuration / 2, delay: 0.0, options: .curveEaseIn, animations: {
                                    self.silently().backgroundColor = self.unselectedBackgroundColor
                                }, completion: nil)
                            }
                            else
                            {
                                self.automaticUnhighlight = false
                            }
                        })
                    }
                    else
                    {
                        self.silently().backgroundColor = self.selectedBackgroundColor
                        
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1, execute: {
                            if(!self.isHighlighted)
                            {
                                self.silently().backgroundColor = self.unselectedBackgroundColor
                            }
                            else
                            {
                                self.automaticUnhighlight = false
                            }
                        })
                    }
                }
                else
                {
                    //Check if the operation was set to animate
                    if(animated || animateAll)
                    {
                        if(!automaticUnhighlight)
                        {
                            UIView.animate(withDuration: animationDuration, delay: 0.0, options: .curveEaseIn, animations: {
                                self.silently().backgroundColor = self.unselectedBackgroundColor
                            }, completion: nil)
                            
                            automaticUnhighlight = true
                        }
                    }
                    else
                    {
                        if(!automaticUnhighlight)
                        {
                            self.silently().backgroundColor = self.unselectedBackgroundColor
                            
                            automaticUnhighlight = true
                        }
                    }
                }
            }
            else
            {
                //Check if the cell is about to be highlighted
                if(highlighted)
                {
                    //Check if the operation was set to animate
                    if(animated || animateAll)
                    {
                        //Execute the highlight animation
                        UIView.animate(withDuration: animationDuration / 2, delay: 0.0, options: .curveEaseOut, animations: {
                            self.silently().backgroundColor = self.selectedBackgroundColor
                        }, completion: nil)
                    }
                    else
                    {
                        //Simply change the cell's background to the highlighted background
                        self.silently().backgroundColor = self.selectedBackgroundColor
                    }
                }
                else
                {
                    //Check if the operation was set to animate
                    if(animated || animateAll)
                    {
                        //Execute the unhighlight animation
                        UIView.animate(withDuration: animationDuration / 2, delay: 0.0, options: .curveEaseOut, animations: {
                            self.silently().backgroundColor = self.unselectedBackgroundColor
                        }, completion: nil)
                    }
                    else
                    {
                        //Simply reset the cell's background
                        self.silently().backgroundColor = self.unselectedBackgroundColor
                    }
                }
            }
        }
        else
        {
            silent = false
        }
    }
    
    override func setSelected(_ selected: Bool, animated: Bool)
    {
        if(!silent)
        {
            if(_mode == .toggle && selected)
            {
                super.setSelected(false, animated: animated)
                setToggled(!isToggled, animated: animated)
            }
            else if(_mode != .toggle)
            {
                super.setSelected(selected, animated: animated)
                
                if(selected)
                {
                    if(animated || animateAll)
                    {
                        UIView.animate(withDuration: animationDuration, delay: 0.0, options: .curveEaseOut, animations: {
                            self.silently().backgroundColor = self.selectedBackgroundColor
                        }, completion: nil)
                    }
                    else
                    {
                        self.silently().backgroundColor = selectedBackgroundColor
                    }
                }
                else
                {
                    if(animated || animateAll)
                    {
                        UIView.animate(withDuration: animationDuration, delay: 0.0, options: .curveEaseIn, animations: {
                            self.silently().backgroundColor = self.unselectedBackgroundColor
                        }, completion: nil)
                    }
                    else
                    {
                        self.silently().backgroundColor = unselectedBackgroundColor
                    }
                }
            }
        }
        else
        {
            silent = false
        }
    }
    
    func setToggled(_ toggled: Bool, animated: Bool)
    {
        //Check if the cell is set to silent mode
        if(!silent)
        {
            //Store the new value of "isToggled"
            self.silently().isToggled = toggled
            
            //Check if the cell is about to be toggled on
            if(toggled)
            {
                originalAccessory = self.accessoryType
                
                //Check if the operation was set to animate
                if(animated || animateAll)
                {
                    //Animate the transition
                    UIView.transition(with: self, duration: animationDuration / 2, options: .transitionCrossDissolve, animations: {
                        self.accessoryType = .checkmark
                    }, completion: nil)
                }
                else
                {
                    //Simply set the new accessory
                    self.accessoryType = .checkmark
                }
            }
            else
            {
                //Check if the operation was set to animate
                if(animated || animateAll)
                {
                    //Animate the transition
                    UIView.transition(with: self, duration: animationDuration / 2, options: .transitionCrossDissolve, animations: {
                        self.accessoryType = self.originalAccessory ?? .none
                    }, completion: nil)
                }
                else
                {
                    //Simply set the new accessory
                    self.accessoryType = originalAccessory
                }
            }
        }
        else
        {
            silent = false
        }
    }

    func _init()
    {
        self.selectionStyle = .none
        
        unselectedBackgroundColor = self.backgroundColor
        
        switch _style
        {
        case .dark:
            selectedBackgroundColor = unselectedBackgroundColor?.adding(0.1)
            
            break
            
        case .light:
            selectedBackgroundColor = unselectedBackgroundColor?.substracting(0.1)
            
            break
        }
    }
    
    func silently() -> MWTableViewCell
    {
        silent = true
        return self
    }
}

enum MWTableViewCellMode: Int
{
    case normal
    case toggle
    
    static var count: Int
    {
        return toggle.hashValue + 1
    }
}

enum MWTableViewCellStyle: Int
{
    case dark
    case light
    
    static var count: Int
    {
        return light.hashValue + 1
    }
}

class MWActionTableViewCell: MWTableViewCell
{
    private var targets: [MWActionTableViewCellTarget] = [MWActionTableViewCellTarget]()
    
    override func setSelected(_ selected: Bool, animated: Bool)
    {
        super.setSelected(selected, animated: animated)
        
        if(!silent)
        {
            for target in targets
            {
                target.possiblyExecute(for: .select)
            }
        }
        else
        {
            silent = false
        }
    }
    
    override func setToggled(_ toggled: Bool, animated: Bool)
    {
        super.setToggled(toggled, animated: animated)
        
        for target in targets
        {
            target.possiblyExecute(for: .toggle)
        }
    }
    
    override func setHighlighted(_ highlighted: Bool, animated: Bool)
    {
        super.setHighlighted(highlighted, animated: animated)
        
        if(!silent)
        {
            for target in targets
            {
                target.possiblyExecute(for: .highlight)
            }
        }
        else
        {
            silent = false
        }
    }
    
    func addTarget(_ target: @escaping (MWActionTableViewCell) -> (), for event: MWActionTableViewCellEvent, identifier: String = "<UNNAMED>")
    {
        targets.append(MWActionTableViewCellTarget(target, cell: self, for: event, identifier: identifier))
    }
    
    func removeTarget(_ identifier: String)
    {
        for (i, target) in targets.enumerated()
        {
            if(target.matches(identifier))
            {
                targets.remove(at: i)
            }
        }
    }

    private class MWActionTableViewCellTarget
    {
        private var target: (MWActionTableViewCell) -> ()
        
        let identifier: String
        private let cell: MWActionTableViewCell
        private let event: MWActionTableViewCellEvent
        
        init(_ target: @escaping (MWActionTableViewCell) -> (), cell: MWActionTableViewCell, for event: MWActionTableViewCellEvent, identifier: String)
        {
            self.target = target
            self.cell = cell
            self.event = event
            self.identifier = identifier
        }
        
        func possiblyExecute(for event: MWActionTableViewCellEvent)
        {
            if(self.event == event)
            {
                target(cell)
            }
        }
        
        func matches(_ identifier: String) -> Bool
        {
            return self.identifier == identifier
        }
        
        func shouldExecute(for event: MWActionTableViewCellEvent) -> Bool
        {
            return self.event == event
        }
    }
}

enum MWActionTableViewCellEvent
{
    case select
    case highlight
    case toggle
}
