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
            _style = MWButtonStyle(rawValue: MWUtil.clamp(style - 1, min: 0, max: MWButtonStyle.count))!
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
    
    /// The font size the button uses for its title label.
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
                toggleHighlight(false)
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
    
    /// This boolean indicates that the button should keep the highlighted look until manual update once it has been highlighted.
    var staysHighlighted: Bool = false
    {
        didSet
        {
            if(!staysHighlighted)
            {
                toggleHighlight(false)
            }
        }
    }
    
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
    /// It is called whenever we change one of the IBInspectable variables or whenever we change the button's style.
    private func _init()
    {        
        //Calculate the selected color from the normal color.
        selectedColor = color.adding(0.3)
        
        //Make the button based on its style.
        switch _style
        {
        case .empty:
            //Make the border
            self.layer.cornerRadius = self.frame.height / 8
            self.layer.borderWidth = 1.5

            //Set the colors
            update()
            
            //Set the title label's font
            self.titleLabel?.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightRegular)
            
            //Set title label colors for each state.
            self.setTitleColor(color, for: .normal)
            self.setTitleColor(selectedColor, for: .highlighted)
            self.setTitleColor(selectedColor, for: .selected)
            self.setTitleColor(selectedColor, for: [.highlighted, .selected])
            
            self.setTitleColor(disabledColor, for: .disabled)
            
            break
        case .filled:
            //Reset the button
            self.layer.borderWidth = 0.0
            self.layer.borderColor = UIColor.clear.cgColor
            
            //Make the rounded corners
            self.layer.cornerRadius = self.frame.height / 8
            
            //Set the colors
            update()
            
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
            self.setTitleColor(color, for: .normal)
            self.setTitleColor(selectedColor, for: .highlighted)
            self.setTitleColor(selectedColor, for: .selected)
            self.setTitleColor(selectedColor, for: [.highlighted, .selected])
            
            self.setTitleColor(disabledColor, for: .disabled)
            
            break
        }
        
        //This very specific case needs to be specified because we do not want to hihglight the button when we drag to exit and than drag back to the button again.
        //This is the way it is by default, and we are disabling that by specifying this target.
        //All other state changes are determined by their corresponding `is...` variable.
        self.addTarget(self, action: #selector(touchDragExit), for: .touchDragExit)
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
                self.update()
            }, completion: nil)
            
        }
        else
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.update()
            }, completion: nil)
        }
    }
    
    /// Updates the look of the button based on the `isHighlighted` variable.
    ///
    /// Called from `_init()` to set the button's look to highlighted without animation, or whenever the `isHighlighted` variable changes its value.
    ///
    /// If the case is the latter, the alpha update is animated.
    ///
    /// It transitions to the correct look based on whether the button is highlighted or not.
    ///
    /// The look for the highlighted state basically involves decreasing the button's alpha value, as the iOS default goes.
    private func toggleHighlight(_ touchDragExit: Bool)
    {
        if(self.isHighlighted)
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.update()
                self.alpha = 0.25
            }, completion: nil)
        }
        else
        {
            if(!staysHighlighted || touchDragExit)
            {
                UIView.transition(with: self, duration: 0.2, options: .transitionCrossDissolve, animations: {
                    self.update()
                    self.alpha = 1.0
                }, completion: nil)
            }
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
                self.update()
            }, completion: nil)
        }
        else
        {
            UIView.transition(with: self, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.update()
            }, completion: nil)
        }
    }
    
    /// Updates the colors of the button's based on the button's style and state.
    ///
    /// Called from `_init()` to initialize the colors for the first time, but it is also called from `toggleEnable()` and `toggleSelect()` to update the colors of the button real time.
    ///
    /// Highlight updates are not in this function, because the highlighted look is the same for all button styles (decreased alpha value).
    ///
    /// - See: `toggleHighlight(_:)` for more details.
    private func update()
    {
        //Switch between the button styles
        switch _style
        {
        //When the style is ".empty", we only update the button's border color
        case .empty:
            //Update the color for the enabled state
            if(self.isEnabled)
            {
                //If enabled, check if the button is selected
                if(self.isSelected || self.isHighlighted)
                {
                    //If selected or highlighted, set the color to the selected color
                    self.layer.borderColor = selectedColor.cgColor
                }
                else
                {
                    //If it is not selected, set the color to the normal color
                    self.layer.borderColor = color.cgColor
                }
            }
            else
            {
                //If disabled, set the color to the disabled color
                self.layer.borderColor = disabledColor.cgColor
            }
            
            break
            
        //When the style is ".filled", we only update the background color of the button's layer
        case .filled:
            //Check if button is enabled
            if(self.isEnabled)
            {
                //If enabled, check if the button is selected
                if(self.isSelected || self.isHighlighted)
                {
                    //If selected or highlighted, set the color to the selected color
                    self.layer.backgroundColor = selectedColor.cgColor
                }
                else
                {
                    //If it is not selected, set the color to the normal color
                    self.layer.backgroundColor = color.cgColor
                }
            }
            else
            {
                //If disabled, set the color to the disabled color
                self.layer.backgroundColor = disabledColor.cgColor
            }
            
            break
            
        //When the style is ".noBorder", we do not update anything, because the only thing that the button shows is its title label (which automatically gets updated)
        case .noBorder:
            break
        }
    }
    
    /// Used to update the button's look for the `touchDragExit` control event.
    @objc private func touchDragExit()
    {
        toggleHighlight(true)
    }
}

//MARK: -

/// The enumeration which holds all button styles that currently exist in myWatch.
enum MWButtonStyle: Int
{
    /// Style __empty__ involves having a button where a thin border surrounds the button with the same color as the title label.
    case empty
    
    /// Style __filled__ involves having a button where the entire button is filled with the user-specified color and the title label is colored with the same color as the button's superview's background. (Making it look like that the title label is "cut" out of the button.)
    case filled
    
    /// Style __noBorder__ involves having a button which looks like just the iOS default. There is nothing else on the button besides the title label, and that is colored with the user-specified color.
    case noBorder
    
    /// Holds the total amount of styles in this enumeration.
    ///
    /// This is required to make clamping the value given in `style` in `MWButton` possible.
    static var count: Int
    {
        return self.noBorder.hashValue + 1
    }
}
