//
//  MWSettingsTableViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWSettingsTableViewController: UITableViewController
{
    //MARK: Instance variables
    @IBOutlet weak var tableViewCellHideTabBarTitles: MWActionTableViewCell!
    
    //MARK: - Inherited functions from: UIViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        //Set the targets for the action cells
        tableViewCellHideTabBarTitles.addTarget(toggle_tableViewCellTabBarHidesTitles, for: .toggle, identifier: "MWActionCellTargetHideTabBarTitlesToggle")
        tableViewCellHideTabBarTitles.isToggled = (self.tabBarController?.tabBar as! MWTabBar).hidesTitles
    }

    override func viewWillAppear(_ animated: Bool)
    {
        super.viewWillAppear(animated)
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }

    // MARK: Inherited functions from: UITableViewDataSource
    override func numberOfSections(in tableView: UITableView) -> Int
    {
        return 1
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int
    {
        if(section == 0)
        {
            return 3
        }
        
        return 0
    }
    
    func toggle_tableViewCellTabBarHidesTitles(_ sender: MWActionTableViewCell)
    {
        if(self.tabBarController is MWMainTabBarController && self.tabBarController?.tabBar is MWTabBar)
        {
            if(sender.isToggled)
            {
                (self.tabBarController?.tabBar as? MWTabBar)?.hidesTitles = true
            }
            else
            {
                (self.tabBarController?.tabBar as? MWTabBar)?.hidesTitles = false
            }
        }
    }
}
