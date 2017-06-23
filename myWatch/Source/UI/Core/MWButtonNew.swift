//
//  MWButtonNew.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 22..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWButtonNew: UIButton
{
    @IBInspectable var style: Int = 0
    {
        didSet
        {
            if(style < 0)
            {
                _style = MWButtonStyle(rawValue: 0)!
            }
            else if(style > MWButtonStyle.count)
            {
                _style = MWButtonStyle(rawValue: MWButtonStyle.count)!
            }
            else
            {
                _style = MWButtonStyle(rawValue: style)!
            }
            
            _init()
        }
    }
    
    @IBInspectable var normalColor: UIColor = MWDefaults.Colors.defaultTintColor
    {
        didSet
        {
            _init()
        }
    }
    
    @IBInspectable var disabledColor: UIColor = UIColor.lightGray
    {
        didSet
        {
            _init()
        }
    }
    
    @IBInspectable var borderWidth: CGFloat = 1.5
    {
        didSet
        {
            _init()
        }
    }
    
    @IBInspectable var customFont: Bool = false
    {
        didSet
        {
            _init()
        }
    }
    
    override var buttonType: UIButtonType
    {
        return .custom
    }
    
    override var isEnabled: Bool
    {
        didSet
        {
            if(oldValue != self.isEnabled)
            {
                toggleEnable()
            }
        }
    }
    
    override var isHighlighted: Bool
    {
        didSet
        {
            if(oldValue != self.isHighlighted)
            {
                toggleHighlight()
            }
        }
    }
    
    override var isSelected: Bool
    {
        didSet
        {
            if(oldValue != self.isSelected)
            {
                toggleSelect()
            }
        }
    }
    
    private var _style: MWButtonStyle = .empty
    {
        didSet
        {
            _init()
        }
    }
    
    private var color: UIColor?
    {
        didSet
        {
            _init()
        }
    }
    
    private var selectedColor: UIColor!
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        _init()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        _init()
    }
    
    private func _init()
    {
        selectedColor = normalColor.adding(0.3)
        
        switch _style
        {
        case .empty:
            //Make the border
            self.layer.cornerRadius = self.frame.height / 8
            self.layer.borderWidth = borderWidth
            self.layer.borderColor = color?.cgColor ?? normalColor.cgColor
            
            //Make the title label
            if(!customFont)
            {
                self.titleLabel?.font = UIFont.systemFont(ofSize: 18.0, weight: UIFontWeightRegular)
            }
            
            self.setTitleColor(color ?? normalColor, for: .normal)
            self.setTitleColor(color ?? normalColor, for: .highlighted)
            self.setTitleColor(color ?? normalColor, for: .selected)
            self.setTitleColor(color ?? normalColor, for: [.highlighted, .selected])
            
            self.setTitleColor(disabledColor, for: .disabled)
            
            break
        case .filled:
            //Reset the button
            self.layer.borderWidth = 0.0
            self.layer.borderColor = UIColor.clear.cgColor
            
            //Make the rounded corners
            self.layer.cornerRadius = self.frame.height / 8
            
            //Set the background
            self.layer.backgroundColor = self.color?.cgColor ?? normalColor.cgColor
            
            //Make the title label
            if(!customFont)
            {
                self.titleLabel?.font = UIFont.systemFont(ofSize: 18.0, weight: UIFontWeightRegular)
            }
            
            let color: UIColor? = self.superview?.backgroundColor
            
            self.setTitleColor(color, for: .normal)
            self.setTitleColor(color, for: .highlighted)
            self.setTitleColor(color, for: .selected)
            self.setTitleColor(color, for: [.highlighted, .selected])
            
            self.setTitleColor(color, for: .disabled)

            break
        case .noBorder:
            //Reset the button
            self.layer.cornerRadius = 0.0
            self.layer.borderWidth = 0.0
            self.layer.borderColor = UIColor.clear.cgColor
            self.layer.backgroundColor = UIColor.clear.cgColor
            
            //Make the title label
            self.setTitleColor(color ?? normalColor, for: .normal)
            self.setTitleColor(color ?? normalColor, for: .highlighted)
            self.setTitleColor(color ?? normalColor, for: .selected)
            self.setTitleColor(color ?? normalColor, for: [.highlighted, .selected])
            
            self.setTitleColor(disabledColor, for: .disabled)
            
            if(!customFont)
            {
                self.titleLabel?.font = UIFont.systemFont(ofSize: 18.0, weight: UIFontWeightRegular)
            }
            
            break
        }
        
        self.addTarget(self, action: #selector(toggleHighlight), for: .touchDragExit)
    }
    
    private func toggleEnable()
    {
        if(self.isEnabled)
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.color = self.normalColor
            }, completion: nil)
            
        }
        else
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.color = self.disabledColor
            }, completion: nil)
        }
    }
    
    @objc private func toggleHighlight()
    {
        if(self.isHighlighted)
        {
            UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseIn, animations: {
                self.alpha = 0.5
            }, completion: nil)
        }
        else
        {
            UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: {
                self.alpha = 1.0
            }, completion: nil)
        }
    }
    
    private func toggleSelect()
    {
        if(self.isSelected)
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.color = self.selectedColor
                self.alpha = 1.0
            }, completion: nil)
        }
        else
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.color = self.normalColor
            }, completion: nil)
        }
    }
}

enum MWButtonStyle: Int
{
    case empty
    case filled
    case noBorder
    
    static var count: Int
    {
        return self.noBorder.hashValue
    }
}
