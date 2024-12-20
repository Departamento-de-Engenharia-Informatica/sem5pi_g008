import {Component, OnInit} from '@angular/core';
import {ActivatedRoute, Router} from '@angular/router';
import {StaffService} from '../../../services/StaffService/staff.service';
import {Staff} from '../../../Domain/Staff';
import {CreateStaff} from '../../../Domain/CreateStaff';

@Component({
  selector: 'app-add-staff-profile',
  templateUrl: './add-staff-profile.component.html',
  styleUrl: './add-staff-profile.component.css',
  standalone: false
})
export class AddStaffProfileComponent implements OnInit {

  public newStaffProfile!: CreateStaff;
  public showErrorMessagePopup: boolean = false;
  public errorMessage: string = '';
  public specializations: string[] = [];

  constructor(private router: Router,
              private route: ActivatedRoute,
              private staffService: StaffService
  ) {
  }

  ngOnInit() {
    this.getSpecializations();
    this.newStaffProfile = {} as CreateStaff;
    this.newStaffProfile.Email = '';
    this.newStaffProfile.FirstName = '';
    this.newStaffProfile.LastName = '';
    this.newStaffProfile.PhoneNumber = 0;
    this.newStaffProfile.Specialization = '';
    this.newStaffProfile.LicenseNumber = 0;
  }

  private getSpecializations() {
    this.staffService.getSpecializations().subscribe(
      (response: any) => {
        this.specializations = response;
      },
      (error: { error: any; }) => {
        console.error('Error:', error);
        alert('Error fetching specializations: ' + (error.error || 'An unknown error occurred.'));
      }
    );
  }

  public areAllFieldsFilled(): boolean {
    return this.checkIfFirstNameIsFilled() && this.checkIfLastNameIsFilled() && this.checkIfEmailIsFilled() && this.checkIfPhoneNumberIsFilled() && this.checkIfSpecializationIsFilled() && this.checkIfLicenseNumberIsFilled();
  }

  private checkIfFirstNameIsFilled(): boolean {
    return this.newStaffProfile.FirstName.trim() !== '';
  }

  private checkIfLastNameIsFilled(): boolean {
    return this.newStaffProfile.LastName.trim() !== '';
  }

  private checkIfEmailIsFilled(): boolean {
    return this.newStaffProfile.Email.trim() !== '';
  }

  private checkIfPhoneNumberIsFilled(): boolean {
    return this.newStaffProfile.PhoneNumber != 0;
  }

  private checkIfSpecializationIsFilled(): boolean {
    return this.newStaffProfile.Specialization.trim() !== '';
  }

  private checkIfLicenseNumberIsFilled(): boolean {
    return this.newStaffProfile.LicenseNumber != 0;
  }

  public createStaffProfile() {
    this.staffService.createStaffProfile(this.newStaffProfile).subscribe(
      () => {
        this.router.navigate(['/admin/staff']).then(() => {
          window.location.reload();
        });

      },
      (error) => {
        if (error.status === 600) {

          this.errorMessage = error.error;
          this.showErrorMessagePopup = true;

        } else if (error.status === 601) {

          this.errorMessage = error.error;
          this.showErrorMessagePopup = true;

        } else if (error.status === 602) {

          this.errorMessage = error.error;
          this.showErrorMessagePopup = true;

        } else if (error.status === 603) {

          this.errorMessage = error.error;
          this.showErrorMessagePopup = true;

        } else if (error.status === 604) {

          this.errorMessage = error.error;
          this.showErrorMessagePopup = true;

        } else if (error.status === 605) {

          this.errorMessage = error.error;
          this.showErrorMessagePopup = true;

        } else {

          console.log('Error updating staff profile:', error);
        }
      }
    );
  }


  public closeErrorMessagePopup()
    :
    void {
    this.showErrorMessagePopup = false;
  }
}
