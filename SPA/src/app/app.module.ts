﻿import {NgModule} from '@angular/core';
import {BrowserModule} from '@angular/platform-browser';
import {RouterModule} from '@angular/router';
import {HttpClientModule} from '@angular/common/http';
import {AppComponent} from './app.component';
import {CommonModule} from '@angular/common';
import {ReactiveFormsModule} from '@angular/forms';
import {FormsModule} from '@angular/forms';
import {routes} from './app.routes';

import {HomeComponent} from './Components/Dashboards/home/home.component';
import {AdminMenuComponent} from './Components/Dashboards/admin-home/admin-home.component';
import {MenuBoxComponent} from './Components/Shared/menu-box/menu-box.component';
import {MenuComponent} from './Components/Shared/menu/menu.component';
import {ProfilePictureMenuComponent} from './Components/Shared/profile-picture-menu/profile-picture-menu.component';
import {OperationTypeManagementComponent} from './Components/OperationTypeManagement/operation-type-management/operation-type-management.component';
import {StaffManagementComponent} from './Components/StaffManagement/staff-management/staff-management.component';
import {BaseDashboardComponent} from './Components/Dashboards/base-dashboard/base-dashboard.component';
import {HomeButtonComponent} from './Components/Shared/home-button/home-button.component';
import {AddOperationTypeComponent} from './Components/OperationTypeManagement/add-operation-type/add-operation-type.component';
import {EditOperationTypeComponent} from './Components/OperationTypeManagement/edit-operation-type/edit-operation-type.component';
import {GoBackButtonComponent} from './Components/Shared/go-back-button/go-back-button.component';
import {DeleteStaffProfileButtonComponent} from './Components/StaffManagement/delete-staff-profile-button/delete-staff-profile-button.component';
import {EditStaffProfileButtonComponent} from './Components/StaffManagement/edit-staff-profile-button/edit-staff-profile-button.component';
import {StaffDetailsComponent} from './Components/StaffManagement/staff-details/staff-details.component';
import {StaffProfileListComponent} from './Components/StaffManagement/staff-profile-list/staff-profile-list.component';
import {FilterButtonComponent} from './Components/Shared/filter-button/filter-button.component';
import {ConfirmModalComponent} from './Components/Shared/confirm-modal/confirm-modal.component';
import {EnterFilterNameComponent} from './Components/Shared/enter-filter-name/enter-filter-name.component';
import {ViewOperationTypeComponent} from './Components/OperationTypeManagement/view-operation-type/view-operation-type.component';
import {EditStaffProfileComponent} from './Components/StaffManagement/edit-staff-profile/edit-staff-profile.component';
import {AddStaffProfileComponent} from './Components/StaffManagement/add-staff-profile/add-staff-profile.component';
import {BackgroudCardComponent} from './Components/Shared/backgroud-card/backgroud-card.component';
import {SafeUrlPipe} from './Components/LinkTo3dModule/safeUrlPipe/safe-url.pipe';
import {Connect3dComponent} from './Components/LinkTo3dModule/connect3d/connect3d.component';
import {RegisterPatientProfileComponent} from './Components/PatientManagement/register-patient-profile/register-patient-profile.component';
import {RegisterPatientComponent} from './Components/Patient/register-patient/register-patient.component';
import {
  PatientManagementComponent
} from './Components/PatientManagement/patientManagement/patient-management.component';
import {
  PatientProfileListComponent
} from './Components/PatientManagement/patient-profile-list/patient-profile-list.component';
import {
  PatientProfileDetailsComponent
} from './Components/PatientManagement/patient-profile-details/patient-profile-details.component';
import {
  DeletePatientProfileButtonComponent
} from './Components/PatientManagement/delete-patient-profile-button/delete-patient-profile-button.component';
import {
  DeletePatientAccoutComponent
} from './Components/Patient/delete-patient-accout1/delete-patient-accout.component';
import {
  EditPatientProfileButtonComponent
} from './Components/PatientManagement/edit-patient-profile-button/edit-patient-profile-button.component';
import {
  EditPatientProfileComponent
} from './Components/PatientManagement/edit-patient-profile/edit-patient-profile.component';
import {DoctorMenuComponent} from './Components/Dashboards/doctor-home/doctor-home.component';
import {
  AddOperationRequestComponent
} from './Components/OperationRequestManagement/add-operation-request/add-operation-request.component';
import {
  OperationRequestManagementComponent
} from './Components/OperationRequestManagement/operation-request-management/operation-request-management.component';
import {
  OperationRequestDetailsComponent
} from './Components/OperationRequestManagement/operation-request-details/operation-request-details.component';
import {
  OperationRequestListComponent
} from './Components/OperationRequestManagement/operation-request-list/operation-request-list.component';




@NgModule({
  declarations: [
    AppComponent,
    HomeComponent,
    AdminMenuComponent,
    MenuBoxComponent,
    MenuComponent,
    ProfilePictureMenuComponent,
    OperationTypeManagementComponent,
    StaffManagementComponent,
    BaseDashboardComponent,
    HomeButtonComponent,
    RegisterPatientComponent,
    AddOperationTypeComponent,
    EditOperationTypeComponent,
    GoBackButtonComponent,
    DeleteStaffProfileButtonComponent,
    EditStaffProfileButtonComponent,
    StaffDetailsComponent,
    StaffProfileListComponent,
    FilterButtonComponent,
    ConfirmModalComponent,
    EnterFilterNameComponent,
    ViewOperationTypeComponent,
    EditStaffProfileComponent,
    AddStaffProfileComponent,
    BackgroudCardComponent,
    Connect3dComponent,
    SafeUrlPipe,
    RegisterPatientProfileComponent,
    PatientManagementComponent,
    PatientProfileListComponent,
    PatientProfileDetailsComponent,
    DeletePatientProfileButtonComponent,

    DeletePatientAccoutComponent,

    EditPatientProfileButtonComponent,
    EditPatientProfileComponent,
    DoctorMenuComponent,
    AddOperationRequestComponent,
    OperationRequestManagementComponent,
    OperationRequestDetailsComponent,
    OperationRequestListComponent

  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    RouterModule.forRoot(routes),
    CommonModule,
    ReactiveFormsModule,
    FormsModule
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
}
