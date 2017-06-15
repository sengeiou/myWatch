//
//  MWButton.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 12..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWButton: UIButton
{
    //MARK: Inspectable variables
    @IBInspectable var buttonColor: UIColor = MWDefaults.Colors.defaultTintColor
    {
        didSet
        {
            self.setNeedsDisplay()
            setupTitleLabel()
        }
    }
    
    @IBInspectable var highlightedColor: UIColor = MWDefaults.Colors.defaultBackgroundColor
    {
        didSet
        {
            self.setNeedsDisplay()
            setupTitleLabel()
        }
    }
    
    @IBInspectable var disabledColor: UIColor = UIColor.lightGray
    {
        didSet
        {
            self.setNeedsDisplay()
            setupTitleLabel()
        }
    }
    
    @IBInspectable var borderWidth: CGFloat = 2.5
    {
        didSet
        {
            self.setNeedsDisplay()
            setupTitleLabel()
        }
    }
    
    //MARK: Private variables
    private var wasHighlightedPreviously: Bool = false, wasDisabledPreviously: Bool = false, hasFinishedInAnimation: Bool = false
    
    //MARK: Inherited variables
    override var buttonType: UIButtonType
    {
        return .custom
    }
    
    override open var isHighlighted: Bool
    {
        didSet
        {
            self.setNeedsDisplay()
        }
    }
    
    override open var isSelected: Bool
    {
        didSet
        {
            self.setNeedsDisplay()
        }
    }
    
    override open var isEnabled: Bool
    {
        didSet
        {
            self.setNeedsDisplay()
        }
    }
    
    //MARK: Inherited functions from: UIButton
    override init(frame: CGRect)
    {
        super.init(frame: frame)

        setupTitleLabel()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)

        setupTitleLabel()
    }
        
    override func draw(_ rect: CGRect)
    {
        super.draw(rect)
        
        //DRAW THE BUTTON BASE
        //This is segment is placed here, because if it would be placed in the '.normal' case, in Interface Builder, these settings would not be set.
        self.layer.borderColor = buttonColor.cgColor
        self.layer.borderWidth = borderWidth
        self.layer.cornerRadius = rect.height / 2
        
        if(state == .normal)
        {
            if(wasHighlightedPreviously)
            {
                UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseIn, animations: {
                    self.layer.backgroundColor = UIColor.clear.cgColor
                }, completion: nil)
                
                wasHighlightedPreviously = false
            }
            else if(wasDisabledPreviously)
            {
                UIView.animate(withDuration: 0.08, delay: 0.0, options: .curveEaseIn, animations: {
                    self.layer.borderColor = self.buttonColor.cgColor
                }, completion: nil)
                
                wasDisabledPreviously = false
            }
        }
        else if(self.state == .highlighted)
        {
            UIView.animate(withDuration: 0.08, delay: 0.0, options: .curveEaseOut, animations: {
                self.layer.backgroundColor = self.buttonColor.cgColor
            }, completion: nil)
            
            wasHighlightedPreviously = true
        }
        else if(self.state == .disabled)
        {
            UIView.animate(withDuration: 0.08, delay: 0.0, options: .curveEaseOut, animations: {
                self.layer.borderColor = self.disabledColor.cgColor
            }, completion: nil)
            
            wasDisabledPreviously = true
        }
        
        setupTitleLabel()
        updateTitleLabel()
    }
    
    private func setupTitleLabel()
    {
        MWUtil.execute(ifNotNil: self.titleLabel) { 
            self.titleLabel!.font = UIFont.systemFont(ofSize: 18.0, weight: UIFontWeightSemibold)
            
            self.setTitleColor(self.buttonColor, for: .normal)
            self.setTitleColor(self.highlightedColor, for: .highlighted)
            self.setTitleColor(self.highlightedColor, for: .selected)
            self.setTitleColor(self.highlightedColor, for: [.selected, .highlighted])
            self.setTitleColor(self.disabledColor, for: .disabled)
            
            self.titleLabel!.alpha = 1.0
        }
    }
    
    private func updateTitleLabel()
    {
        MWUtil.execute(ifNotNil: self.titleLabel) { 
            self.titleLabel!.alpha = 1.0
        }
    }
}

extension UIButton
{
    func disableButton()
    {
        self.isEnabled = false
    }
    
    func enableButton()
    {
        self.isEnabled = true
    }
    
    func selectButton()
    {
        self.isSelected = true
    }
    
    func deselectButton()
    {
        self.isSelected = false
    }
}
