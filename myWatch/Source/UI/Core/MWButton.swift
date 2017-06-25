//
//  MWButton.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 22..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWButton: UIButton
{
    //MARK: Inspectables
    
    /// The style of the button in an Interface Builder-supported format.
    ///
    /// We use the number this variable holds to make an MWButtonStyle instance out of it.
    ///
    /// Before we can use the variable, we have to clamp it, so that it does not get out of range of the MWButtonStyle enumeration.
    /// - See: `count` in `MWButtonStyle`
    /// - Also, see `_style` below for more details on the styles.
    @IBInspectable var style: Int = 1
    {
        didSet
        {
            if(style < 1)
            {
                _style = MWButtonStyle(rawValue: 0)!
            }
            else if(style > MWButtonStyle.count)
            {
                _style = MWButtonStyle(rawValue: MWButtonStyle.count - 1)!
            }
            else
            {
                _style = MWButtonStyle(rawValue: style - 1)!
            }
            
            _init()
        }
    }
    
    /// The overall color of the button.
    ///
    /// This variable is the original color that the button will be colored with at intialization, and the color it will be recolored to when resuming the unselected/unhighlighted (normal) state.
    ///
    /// We use this variable to calculate the color for the selected state of the button. 
    /// - See: `_init()` for more information on the calculation.
    @IBInspectable var color: UIColor = MWDefaults.Colors.defaultTintColor
    {
        didSet
        {
            _init()
        }
    }
    
    /// The button's color for the disabled state.
    @IBInspectable var disabledColor: UIColor = UIColor.lightGray
    {
        didSet
        {
            _init()
        }
    }
    
    /// This variable holds a boolean which determines whether a custom font could be used for a button or not.
    ///
    /// If false, the button will use a default font.
    ///
    /// If true, the button will use the font specified in Interface Builder.
    ///
    /// - See: `_init()` for more information on the font used.
    @IBInspectable var fontSize: CGFloat = 20.0
    {
        didSet
        {
            _init()
        }
    }
    
    //MARK: Overriden variables
    
    /// Overriden, because we must have the button type set to this in order to stop the default animation of the title label.
    override var buttonType: UIButtonType
    {
        get
        {
            return .custom
        }
    }
    
    /// Whenever the `isEnabled` variable changes its value, we assume that the button needs to change its look based on its value.
    ///
    /// Whenever the variable is set, we check whether the value has changed (in comparison of the old value), and if it has, we update the button's look.
    ///
    /// - See: `toggleEnable()` for more details on the look.
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
    
    /// Whenever the `isHighlighted` variable changes its value, we assume that the button needs to change its look based on its value.
    ///
    /// Whenever the variable is set, we check whether the value has changed (in comparison of the old value), and if it has, we update the button's look.
    ///
    /// - See: `toggleHighlight()` for more details on the look.
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
    
    /// Whenever the `isSelected` variable changes its value, we assume that the button needs to change its look based on its value.
    ///
    /// Whenever the variable is set, we check whether the value has changed (in comparison of the old value), and if it has, we update the button's look.
    ///
    /// - See: `toggleSelect()` for more details on the look.
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
    
    //MARK: Instance variables
    
    /// Holds the actual style of the button which later will be used to determine which style we should draw.
    ///
    /// - See: `MWButtonStyle` for more details on the styles.
    private var _style: MWButtonStyle = .empty
    {
        didSet
        {
            _init()
        }
    }
    
    /// Holds the current color of the button.
    ///
    /// Whenever the button state is updated, we change this variable to change the entire look of the button.
    ///
    /// This is possible, because whenever we change this variable `_init()` gets called which recolors and remakes the button.
    ///
    /// The `_init()` method can be called by two sources which need to recolor the button.
    ///
    /// The first one is the `color` variable which holds the overall color of the button in its normal state.
    /// When we change this variable, we have to recolor, because we likely change its value from Interface Builder and we have to make the changes to the button, otherwise the custom set color would not be set as the button's color (not just in Interface Builder, but even in runtime).
    ///
    /// The second one is the `currentColor` variable itself which may only be changed whenever we highlight or disable the button.
    /// We must recolor again, because we need to see the coloring changes upon disabling or highlighting the button.
    private var currentColor: UIColor?
    {
        didSet
        {
            _init()
        }
    }
    
    /// This variable holds the button's color for the selected state.
    /// It will dynamically be calculated from the `color` variable whenever `_init()` is called.
    private var selectedColor: UIColor!
    
    //MARK: - Inherited functions from: UIButton
    
    /// Basic initializer which calls our own designated method to initialize the button.
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        _init()
    }
    
    /// Required initializer which calls our own designated method to initialize the button.
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        _init()
    }
    
    //MARK: Instance functions
    
    /// Our own initializer method designated to recolor and remake the button.
    ///
    /// It is called whenever we change one of the IBInspectable variables or whenever we have to recolor or remake the button. For example, when its state is changed.
    ///
    /// Beacuse recoloring can be from two different sources (see `currentColor` above), the method is designed to use the `color` variable if `currentColor` is nil.
    ///
    /// `currentColor` is nil by default and is given a value when we animate. This way we can actually tell where the method was called from.
    private func _init()
    {
        toggleHighlight()
        
        //Calculate the selected color from the normal color.
        selectedColor = color.adding(0.3)
        
        //Make the button based on its style.
        switch _style
        {
        case .empty:
            //Make the border
            self.layer.cornerRadius = self.frame.height / 8
            self.layer.borderWidth = 1.5
            self.layer.borderColor = currentColor?.cgColor ?? color.cgColor
            
            //Set the title label's font
            self.titleLabel?.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightRegular)
            
            //Set title label colors for each state.
            self.setTitleColor(currentColor ?? color, for: .normal)
            self.setTitleColor(currentColor ?? color, for: .highlighted)
            self.setTitleColor(currentColor ?? color, for: .selected)
            self.setTitleColor(currentColor ?? color, for: [.highlighted, .selected])
            
            self.setTitleColor(disabledColor, for: .disabled)
            
            break
        case .filled:
            //Reset the button
            self.layer.borderWidth = 0.0
            self.layer.borderColor = UIColor.clear.cgColor
            
            //Make the rounded corners
            self.layer.cornerRadius = self.frame.height / 8
            
            //Set the background
            self.layer.backgroundColor = currentColor?.cgColor ?? self.color.cgColor
            
            //Set the title label's font
            self.titleLabel?.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightRegular)
            
            //Set the color of the title label for each state
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
            
            //Set the title label's font
            self.titleLabel?.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightRegular)
            
            //Set the color of the title label for each state
            self.setTitleColor(currentColor ?? color, for: .normal)
            self.setTitleColor(currentColor ?? color, for: .highlighted)
            self.setTitleColor(currentColor ?? color, for: .selected)
            self.setTitleColor(currentColor ?? color, for: [.highlighted, .selected])
            
            self.setTitleColor(disabledColor, for: .disabled)
            
            break
        }
        
        //This very specific case needs to be specified because we do not want to hihglight the button when we drag to exit and than drag back to the button again.
        //This is the way it is by default, and we are disabling that by specifying this target.
        //All other state changes are determined by their corresponding `is...` variable.
        self.addTarget(self, action: #selector(toggleHighlight), for: .touchDragExit)
    }
    
    /// Updates the look of the button based on the `isEnabled` variable.
    ///
    /// Called whenever the `isEnabled` variable changes its value.
    ///
    /// It transitions to the correct look based on whether the button is enabled or not.
    ///
    /// The look for the disabled state basically involves changing the button's coloring to a user-specified disabled color.
    ///
    /// - See: `disabledColor` for more details.
    private func toggleEnable()
    {
        if(self.isEnabled)
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.currentColor = self.color
            }, completion: nil)
            
        }
        else
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.currentColor = self.disabledColor
            }, completion: nil)
        }
    }
    
    /// Updates the look of the button based on the `isHighlighted` variable.
    ///
    /// Called whenever the `isHighlighted` variable changes its value.
    ///
    /// It transitions to the correct look based on whether the button is highlighted or not.
    ///
    /// The look for the highlighted state basically involves decreasing the button's alpha value, as the iOS default goes.
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
    
    /// Updates the look of the button based on the `isSelected` variable.
    ///
    /// Called whenever the `isSelected` variable changes its value.
    ///
    /// It transitions to the correct look based on whether the button is selected or not.
    ///
    /// The look for the selected state basically involves changing the button's coloring to a dynamically calculated selected color based on the given button color by the user.
    /// - See: `selectedColor` for more details.
    private func toggleSelect()
    {
        if(self.isSelected)
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.currentColor = self.selectedColor
                self.alpha = 1.0
            }, completion: nil)
        }
        else
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.currentColor = self.color
            }, completion: nil)
        }
    }
}

//MARK: -

/// The enumeraion which holds all button styles that currently exist in myWatch.
enum MWButtonStyle: Int
{
    /// Style __empty__ involves having a button where a thin border surrounds the button with the same color as the title label.
    case empty
    
    /// Style __filled__ involves having a button where the entire button is filled with the user-specified color and the title label is colored with the same color as the button's superview's background. (Making it look like that the title label is "cut" out of the button.)
    case filled
    
    /// Style __noBorder__ involves having a button which looks like just the iOS default. There is nothing else on the button besides the title label, and that is colored with the user-specified color.
    case noBorder
    
    ///Holds the total amount of styles in this enumeration.
    ///
    ///This is required to make clamping the value given in `style` in `MWButton` possible.
    static var count: Int
    {
        return self.noBorder.hashValue + 1
    }
}
