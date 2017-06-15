//
//  MWTextField.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWTextField: UITextField
{
    @IBInspectable var color: UIColor = MWDefaults.Colors.defaultTextFieldColor
    @IBInspectable var _textColor: UIColor = UIColor.white
    
    private var highlightedColor: UIColor = UIColor()
    private var disabledColor: UIColor = UIColor()
    private var disabledTextColor: UIColor = UIColor.black
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        self.tintColor = _textColor
        
        highlightedColor = UIColor(red: color.getComponentRed() + 0.0269, green: color.getComponentGreen() + 0.0269, blue: color.getComponentBlue() + 0.0269, alpha: color.getComponentAlpha())
        disabledColor = UIColor(red: color.getComponentRed() + 0.0538, green: color.getComponentGreen() + 0.0538, blue: color.getComponentBlue() + 0.0538, alpha: color.getComponentAlpha())
        
        self.addTarget(self, action: #selector(update), for: UIControlEvents.editingDidBegin)
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        self.tintColor = _textColor
        
        highlightedColor = UIColor(red: color.getComponentRed() + 0.0269, green: color.getComponentGreen() + 0.0269, blue: color.getComponentBlue() + 0.0269, alpha: color.getComponentAlpha())
        disabledColor = UIColor(red: color.getComponentRed() + 0.0538, green: color.getComponentGreen() + 0.0538, blue: color.getComponentBlue() + 0.0538, alpha: color.getComponentAlpha())
        
        self.addTarget(self, action: #selector(update), for: UIControlEvents.editingDidBegin)
    }
    
    override func textRect(forBounds bounds: CGRect) -> CGRect
    {
        return CGRect(x: bounds.origin.x + 15, y: bounds.origin.y + 1.5, width: bounds.width - 30, height: bounds.height)
    }
    
    override func editingRect(forBounds bounds: CGRect) -> CGRect
    {
        return CGRect(x: bounds.origin.x + 15, y: bounds.origin.y + 1.5, width: bounds.width - 30, height: bounds.height)
    }
    
    override func placeholderRect(forBounds bounds: CGRect) -> CGRect
    {
        return CGRect(x: bounds.origin.x + 15, y: bounds.origin.y + 1.5, width: bounds.width - 30, height: bounds.height)
    }
    
    override func draw(_ rect: CGRect) 
    {
        self.layer.cornerRadius = rect.height / 2
        
        if(self.state == .normal)
        {
            UIView.animate(withDuration: 0.2, delay: 0.0, options: .curveEaseIn, animations: {
                self.backgroundColor = self.color
                self.textColor = self._textColor
            }, completion: nil)
            
            self.attributedPlaceholder = makeAttributedPlaceholder(for: .normal, withAlpha: 0.3)
        }
        else if(self.isFirstResponder) //aka. if we're editing.
        {
            UIView.animate(withDuration: 0.2, delay: 0.0, options: .curveEaseOut, animations: {
                self.backgroundColor = self.highlightedColor
                self.textColor = self._textColor
            }, completion: nil)
            
            self.attributedPlaceholder = nil
        }
        else if(state == .disabled)
        {
            UIView.animate(withDuration: 0.2, delay: 0.0, options: .curveEaseOut, animations: {
                self.backgroundColor = self.disabledColor
                self.textColor = self.disabledTextColor
            }, completion: nil)
            
            self.attributedPlaceholder = makeAttributedPlaceholder(for: .disabled, withAlpha: 1.0)
        }
    }
    
    @objc private func update()
    {
        self.setNeedsDisplay()
    }
    
    private func makeAttributedPlaceholder(for state: UIControlState, withAlpha alpha: CGFloat) -> NSAttributedString
    {
        var ret: NSAttributedString = NSAttributedString(string: self.placeholder != nil ? self.placeholder! : "")
        
        if(state == .normal)
        {
            ret = NSAttributedString(string: self.placeholder != nil ? self.placeholder! : "", attributes: [NSForegroundColorAttributeName : _textColor.withAlphaComponent(alpha)])
        }
        else if(state == .disabled)
        {
            ret = NSAttributedString(string: self.placeholder != nil ? self.placeholder! : "", attributes: [NSForegroundColorAttributeName : disabledTextColor.withAlphaComponent(alpha)])
        }
        
        return ret
    }
}
