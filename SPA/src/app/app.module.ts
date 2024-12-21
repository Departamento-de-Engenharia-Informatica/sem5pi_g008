import {NgModule} from '@angular/core';
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
import {PatientManagementComponent} from './Components/PatientManagement/patientManagement/patient-management.component';
import {PatientProfileListComponent} from './Components/PatientManagement/patient-profile-list/patient-profile-list.component';
import {PatientProfileDetailsComponent} from './Components/PatientManagement/patient-profile-details/patient-profile-details.component';
import {DeletePatientProfileButtonComponent} from './Components/PatientManagement/delete-patient-profile-button/delete-patient-profile-button.component';
import {UpdatePatientAccoutComponent} from "./Components/Patient/update-patient-accout/update-patient-accout.component";
import {ListOperationRequestComponent} from "./Components/OperationRequest/list-operation-request/list-operation-request.component";
import {DeletePatientAccoutComponent} from './Components/Patient/delete-patient-accout1/delete-patient-accout.component';
import {EditPatientProfileButtonComponent} from './Components/PatientManagement/edit-patient-profile-button/edit-patient-profile-button.component';
import {EditPatientProfileComponent} from './Components/PatientManagement/edit-patient-profile/edit-patient-profile.component';
import {StaffHomeComponent} from './Components/Dashboards/staff-home/staff-home.component';
import { PatientHomeComponent } from './Components/Dashboards/patient-home/patient-home.component';
import { CheckUserToDeleteComponent } from './Components/Patient/check-user-to-delete/check-user-to-delete.component';
import {EditOperationRequestComponent} from './Components/OperationRequest/edit-operation-request/edit-operation-request.component';
import {AddOperationRequestComponent} from './Components/OperationRequest/add-operation-request/add-operation-request.component';
import { PatientProfileComponent } from './Components/Patient/patient-profile/patient-profile.component';
import {AddAllergyComponent} from './Components/AllergyManagement/add-allergy/add-allergy.component';
import {SpecializationManagementComponent} from './Components/SpecializationManagement/specialization-management/specialization-management.component';
import {SpecializationListComponent} from './Components/SpecializationManagement/specialization-list/specialization-list.component';
import { ResetFilterButtonComponent } from './Components/Shared/reset-filter-button/reset-filter-button.component';
import {ErrorMessageComponent} from './Components/Shared/error-message/error-message.component';
import {DeleteButtonComponent} from './Components/Shared/delete-button/delete-button.component';
import { CreateSpecializationComponent } from './Components/SpecializationManagement/create-specialization/create-specialization.component';
import { AddMedicalConditionComponent } from './Components/MedicalConditionManagement/add-medical-condition/add-medical-condition.component';
import {AllergyManagementComponent} from './Components/AllergyManagement/allergy-management/allergy-management.component';
import {PatientManagementAsDoctorComponent} from './Components/PatientManagementAsADoctor/patientManagementAsDoctor/patient-management.component-as-doctor';
import {
  CreateAppointmentComponent
} from './Components/Appointment/createAppointment/create-appointment/create-appointment.component';
import {
  EditSpecializationComponent
} from './Components/SpecializationManagement/edit-specialization/edit-specialization.component';

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
    DeleteButtonComponent,
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
    UpdatePatientAccoutComponent,
    EditPatientProfileButtonComponent,
    EditPatientProfileComponent,
    ListOperationRequestComponent,
    DeletePatientAccoutComponent,
    EditPatientProfileButtonComponent,
    EditPatientProfileComponent,
    StaffHomeComponent,
    PatientHomeComponent,
    CheckUserToDeleteComponent,
    EditOperationRequestComponent,
    AddOperationRequestComponent,
    PatientProfileComponent,
    AddAllergyComponent,
    SpecializationManagementComponent,
    ErrorMessageComponent,
    SpecializationListComponent,
    ResetFilterButtonComponent,
    CreateSpecializationComponent,
    AddMedicalConditionComponent
    AllergyManagementComponent,
    PatientManagementAsDoctorComponent,
    CreateAppointmentComponent,
    EditSpecializationComponent
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
