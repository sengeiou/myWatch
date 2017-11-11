//
//  MWTabBar.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 13..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// The custom-designed tab bar of the application.
@IBDesignable
class MWTabBar: UITabBar
{
    //MARK: Inspectables
    
    /// An Interface Builder-compatible representation of the style of the tab bar.
    ///
    /// Whenever it is set, the tab bar will clamp its value, so it does not go out of bounds when instantiating an `MWTabBarStyle`.
    ///
    /// The variable's value is provided as the raw value of the new `MWTabBarStyle` instance.
    @IBInspectable var style: Int = 1
    {
        didSet
        {
            //Create the instance from the clamped value
            _style = MWTabBarStyle(rawValue: MWUtil.clamp(style - 1, min: 0, max: MWTabBarStyle.count))! //It should not be nil, because the raw value is clamped between the minimum (0) and the maximum value (declared in "MWTabBarStyle").
        }
    }
    
    /// A boolean which indicates whether the tab bar should hide the titles of the items.
    ///
    /// Whenever it is set, the tab bar either hides or restores the titles.
    ///
    /// If the case is the latter, the tab bar will restore the titles from the `titles` array.
    @IBInspectable var hidesTitles: Bool = false
    {
        didSet
        {
            //Check if the value has changed
            if(oldValue != hidesTitles)
            {
                //Update the titles based on the new value
                updateTitles()
            }
        }
    }
    
    //MARK: Instance variables
    
    /// The style of the tab bar.
    ///
    /// Whenever it is set, the tab bar will redraw/update itself to the new look.
    ///
    ///  - For more information on the styles, see: `MWTabBarStyle`.
    internal var _style: MWTabBarStyle = .system
    {
        didSet
        {
            //Update/redraw the tab bar based on the new value.
            _init()
        }
    }
    
    /// The corresponding controls for every item of the tab bar.
    ///
    /// Whenever items are modified, this array modifies as well, but only if the count of the items change.
    ///
    /// The controls held in this array allow the tab bar to have access to the internal view of a tab bar item.
    ///
    /// To retrieve the image view which displays the tab bar item's icon:
    ///
    ///     let imageViewIcon: UIImageView = controls[0].imageView
    ///
    /// To retrieve the label which displays the title of the tab bar item:
    ///
    ///     let labelTitle: UILabel = controls[0].label
    ///
    private var controls: [MWTabBarControl] = [MWTabBarControl]()
    
    /// Holds the control of the currently selected item.
    private var selectedControl: UIControl?
    
    /// An integer whose value indicates whether the next animatable operation of the tab bar should be animated.
    ///
    /// If its value is `0`, the tab bar will not animate at all.
    ///
    /// If its value is `1`, or more, it will animate operations with each operation decreasing its value by 1. If it reaches `0`, the tab bar will not animate.
    ///
    /// Outside of this class, its value may be increased by 1 using modifier `animating()`.
    ///
    /// Example (from a tab bar controller):
    ///
    ///     myTabBar.hidesTitles = true //Will not animate
    ///
    ///     myTabBar.animating().hidesTitles = true //Will animate
    ///
    ///     myTabBar.animating().animating().hidesTitles = true //Will animate
    ///
    ///     myTabBar.setItems(newItems, animated: true/false) //Will animate no matter of the "animated" parameter, because in the previous line, 2 animating operations were requested.
    ///
    private var animated: Int = 0
    
    private var shadowLayer: CALayer = CALayer()
    
    private var clippingLayer: CALayer = CALayer()
    
    private var removedSeparatorLine: Bool = false
    
    //MARK: - Inherited initializers from: UITabBar
    override init(frame: CGRect)
    {
        //Supercall
        super.init(frame: frame)
        
        //Initialize using custom initializer
        _init()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        //Supercall
        super.init(coder: aDecoder)
        
        //Initialize using custom initializer
        _init()
    }
    
    //MARK: Inherited functions from: UITabBar
    override func setItems(_ items: [UITabBarItem]?, animated: Bool)
    {
        //Check if we should animate
        if(self.animated >= 1)
        {
            //Supercall
            super.setItems(items, animated: true)
            
            //Decrease "(self.)animated"
            self.animated -= 1
        }
        else
        {
            //Supercall
            super.setItems(items, animated: false)
        }
        
        //Iterate though the subviews and get the titles of the items if the the titles array has not yet been initialized, or if the the number of items changed.
        //NOTE: In the supercall, views for the items, provided in the "items" parameter, are created and added to the subviews of the tab bar
        //      These views are all inherit "UIControl", and have two subviews: the icon (image view) of the item, and its title.
        if(controls.count == 0)
        {
            //Iterate through the subviews
            for subview in self.subviews
            {
                //Check if the subview is one of the views of the items
                if(subview is UIControl)
                {
                    let control: MWTabBarControl = MWTabBarControl(control: subview as! UIControl)
                    control.control.addTarget(self, action: #selector(selectItem(control:)), for: .touchUpInside)
                    
                    controls.append(control)
                }
            }
        }
        else if(controls.count != self.items?.count)
        {
            //Check which direction the count has changed
            if(controls.count < self.items!.count)
            {
                //Iterate through the subviews
                for subview in self.subviews
                {
                    //Check if the subview is one of the views of the items
                    if(subview is UIControl)
                    {
                        //Create a new "MWTabBarItemControl" instance out of this control
                        let control: MWTabBarControl = MWTabBarControl(control: subview as! UIControl)
                        control.control.addTarget(self, action: #selector(selectItem(control:)), for: .touchUpInside)
                        
                        if(!controls.contains(control))
                        {
                            controls.append(control)
                        }
                    }
                }
            }
            else if(controls.count > self.items!.count) //"items" cannot be nil, because if it would be, this block would not be executed
            {
                //Iterate through the controls
                //NOTE: The "itemControls" array at this point still contains the control of the removed item.
                for control in controls
                {
                    //Check if the control is added to the view hierarchy. If the control is not added to it, it is likely that the current item is a removed item
                    if(!control.isInViewHierarchy())
                    {
                        //Remove the title if it is not in the hierarchy anymore
                        controls.remove(at: controls.index(of: control)!)
                    }
                }
            }
        }
    }
    
    override func layoutSubviews()
    {
        //Supercall
        super.layoutSubviews()
        
        //Call the function with the layout implementation
        self.layoutIfNeeded()
    }
    
    override func layoutIfNeeded()
    {
        //Supercall
        super.layoutIfNeeded()
        
        //Update the titles if necessary
        updateTitles()
        
        //Update the style of the button if necessary
        //Update/redraw the custom tab bar if the style is custom
        if(_style == .custom)
        {
            //Remove the separator line if we have not removed it already
            if(!removedSeparatorLine)
            {
                if let separatorLine = getSeparatorLine(for: self)
                {
                    separatorLine.isHidden = true
                    removedSeparatorLine = true
                }
            }
            
            //Update/redraw
            clippingLayer.frame = self.bounds.offsetBy(dx: 0.0, dy: -(self.bounds.height -- 30.0)).withSize(width: self.bounds.width, height: 30.0)
            
            let shadowPath = UIBezierPath(rect: self.bounds.scaleBy(width: -10.0, height: 0.0))
            shadowLayer.frame = clippingLayer.bounds.offsetBy(dx: 0.0, dy: clippingLayer.bounds.height)
            
            shadowLayer.shadowOffset = CGSize(width: 5.0, height: -4.0)
            shadowLayer.shadowPath = shadowPath.cgPath
        }
        
    }
    
    //MARK: Instance functions
    
    /// A modifier which forces the tab bar to perform the next redraw/update animated.
    ///
    /// - Returns: This tab bar.
    func animating() -> MWTabBar
    {        
        animated += 1
        return self
    }
    
    /// The tab bar's custom initializer.
    ///
    /// Initializes the tab bar based on its style.
    private func _init()
    {
        //Check which style the tab bar is set to
        switch _style
        {
        //For case "system" we do not do anything - leave the system default
        case .system:
            break
        
        //For case "custom" we draw a shadow, which is clipped outside of the tab bar, so it does not effect the translucency
        case .custom:
            //Set the tab bar's layer allow drawing outside of its region
            self.layer.masksToBounds = false
            
            //Setup the layer which the shadow will be clipped to
            clippingLayer.frame = self.bounds.offsetBy(dx: 0.0, dy: -(self.bounds.height -- 30.0)).withSize(width: self.bounds.width, height: 30.0)
            clippingLayer.masksToBounds = true
            
            //Create the shadow's layer
            let shadowPath = UIBezierPath(rect: self.bounds.scaleByCentered(width: -10.0, height: 0.0))
            shadowLayer.frame = clippingLayer.bounds.offsetBy(dx: 0.0, dy: clippingLayer.bounds.height)
            
            //Setup the shadow
            shadowLayer.shadowColor = UIColor.black.cgColor
            shadowLayer.shadowRadius = 7.0
            shadowLayer.shadowOpacity = 0.5
            shadowLayer.shadowOffset = CGSize(width: 5.0, height: -4.0)
            shadowLayer.shadowPath = shadowPath.cgPath
            shadowLayer.masksToBounds = false
            
            //Clip the shadow by adding it to the clipping layer, then add the final result to the tab bar's layer
            clippingLayer.addSublayer(shadowLayer)
            self.layer.addSublayer(clippingLayer)
            
            break
        }
    }
    
    /// Updates the titles of the tab bar based on the value of `hidesTitles`.
    ///
    /// If the tab bar does not hide the titles, but it did previously, it will restore them from the `titles` array.
    ///
    /// Otherwise, the tab bar will simply remove the titles from its subviews.
    private func updateTitles()
    {
        for control in controls
        {
            if(animated >= 1)
            {
                UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseOut, animations: { 
                    control.imageView.frame = control.control.frame.withPosition(x: control.control.frame.origin.x, y: self.hidesTitles ? ((self.frame.height - control.imageView.frame.height) / 2) - control.imageView.frame.origin.y : 1)
                    
                    control.label.isHidden = self.hidesTitles
                }, completion: nil)
            }
            else
            {
                //Reposition the image view
                control.control.frame = control.control.frame.withPosition(x: control.control.frame.origin.x, y: hidesTitles ? ((self.frame.height - control.imageView.frame.height) / 2) - control.imageView.frame.origin.y : 1)
                
                //Hide/show the title
                control.label.isHidden = hidesTitles
            }
        }
        
        if(animated >= 1)
        {
            animated -= 1
        }
    }
    
    /// Animates the selection of the control provided in the parameter and the unselection of the currently selected control.
    ///
    /// - Parameter control: The control which is about to be selected.
    @objc private func selectItem(control: UIControl)
    {
        //Animate the selection of the new control
        UIView.transition(with: control, duration: 0.1, options: .transitionCrossDissolve, animations: { 
            control.isSelected = true
        }, completion: nil)
        
        //Check if we have a currently selected control
        selectedControl ?! {
            //Animate the unselection of the currently selected control
            UIView.transition(with: self.selectedControl!, duration: 0.1, options: .transitionCrossDissolve, animations: {
                self.selectedControl!.isSelected = false
            }, completion: nil)
        }
        
        //Set the new control as the selected control
        selectedControl = control
    }
    
    /// Searches for a separator line in subviews of the given view
    ///
    /// - Parameter view: The view whose subviews the function should search for the separator line in.
    /// - Returns: The separator line (default shadow image) of the tab bar.
    private func getSeparatorLine(for view: UIView) -> UIImageView?
    {
        //Check if the current view is the separator line
        if(view is UIImageView && view.frame.height <= 1)
        {
            //If it is, return it as a "UIImageView"
            return view as? UIImageView
        }
        
        //If it is not, search for it in its subviews
        for subview in view.subviews
        {
            //For optimization puposes, we exclude the buttons from the search
            //(We are looking for a view with type "_UIBarBackground", but that is not a public view type available in UIKit.)
            if(!(subview is UIControl))
            {
                if let shadowImage = getSeparatorLine(for: subview)
                {
                    return shadowImage
                }
            }
        }
        
        return nil
    }
}

/// A basic class which collects the two main views of a tab bar item control and the control itself.
fileprivate class MWTabBarControl: NSObject
{
    /// The corresponding control of the tab bar item.
    var control: UIControl
    
    /// The image view which displays the icon of the tab bar item.
    var imageView: UIImageView
    
    /// The label which displays the title of the tab bar item.
    var label: UILabel
    
    /// Makes an `MWTabBarControl` instance out of the given parameters.
    ///
    /// - Parameter control: The control which must have an image view as its subview at slot `0` and a label at slot `1`. (Can be directly extracted from the subviews of any tab bar.)
    ///
    ///   __NOTE:__ This specific control type is called `UITabBarButton`, but it is not released in the public version of UIKit. Therefore, the way we check whether a control is an instance of this type is by checking whether the control's structure matches the structure of a `UITabBarButton`.
    init(control: UIControl)
    {
        //Store the control
        self.control = control
        
        //Check if the control is an item's control directly extracted from the tab bar.
        if(control.subviews[0] is UIImageView && control.subviews[1] is UILabel)
        {
            //Store the image view and the label
            self.imageView = control.subviews[0] as! UIImageView
            self.label = control.subviews[1] as! UILabel
        }
        else
        {
            //Assert the application if the control is not compatible
            fatalError("The control provided in the initializer of \"MWTabBarItemControl\" does not seem like it's an instance of \"UITabBarButton\"!")
        }
    }
    
    /// Checks and returns whether the control is in the view hierarchy.
    ///
    /// - Returns: A boolean which indicates whether the control is in the view hierarchy.
    func isInViewHierarchy() -> Bool
    {
        return control.window != nil
    }
}

/// The enumeration which holds all tab bar styles that currently exist in myWatch.
enum MWTabBarStyle: Int
{
    /// Case __system__ involves having a tab bar which looks like the system default
    case system
    
    /// Case __custom__ involves having a tab bar with a custom shadow and with the 1px separator line removed.
    case custom
    
    ///Holds the total amount of styles in this enumeration.
    ///
    ///This is required to make clamping the value given in `style` in `MWTabBar` possible.
    static var count: Int
    {
        return self.custom.hashValue + 1
    }
}
