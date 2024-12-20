import { Routes } from '@angular/router';
import appSettings from './appsettings.json';
import { HomeComponent } from './Components/Dashboards/home/home.component';
import { AdminMenuComponent } from './Components/Dashboards/admin-home/admin-home.component';
import { OperationTypeManagementComponent } from './Components/OperationTypeManagement/operation-type-management/operation-type-management.component';
import { StaffManagementComponent } from './Components/StaffManagement/staff-management/staff-management.component';
import { AddOperationTypeComponent } from './Components/OperationTypeManagement/add-operation-type/add-operation-type.component';
import { EditOperationTypeComponent } from './Components/OperationTypeManagement/edit-operation-type/edit-operation-type.component';
import { ViewOperationTypeComponent } from './Components/OperationTypeManagement/view-operation-type/view-operation-type.component';
import { EditStaffProfileComponent } from './Components/StaffManagement/edit-staff-profile/edit-staff-profile.component';
import { AddStaffProfileComponent } from './Components/StaffManagement/add-staff-profile/add-staff-profile.component';
import { Connect3dComponent } from './Components/LinkTo3dModule/connect3d/connect3d.component';
import { RegisterPatientProfileComponent } from './Components/PatientManagement/register-patient-profile/register-patient-profile.component';
import { PatientManagementComponent } from './Components/PatientManagement/patientManagement/patient-management.component';
import { AuthGuard } from './Guard/auth.guard';
import { UpdatePatientAccoutComponent } from './Components/Patient/update-patient-accout/update-patient-accout.component';
import { ListOperationRequestComponent } from './Components/OperationRequest/list-operation-request/list-operation-request.component';
import { DeletePatientAccoutComponent } from './Components/Patient/delete-patient-accout1/delete-patient-accout.component';
import { EditPatientProfileComponent } from './Components/PatientManagement/edit-patient-profile/edit-patient-profile.component';
import {StaffHomeComponent} from './Components/Dashboards/staff-home/staff-home.component';
import { PatientHomeComponent } from './Components/Dashboards/patient-home/patient-home.component';
import { CheckUserToDeleteComponent } from './Components/Patient/check-user-to-delete/check-user-to-delete.component';
import { AddOperationRequestComponent } from './Components/OperationRequest/add-operation-request/add-operation-request.component';
import { PatientProfileComponent } from './Components/Patient/patient-profile/patient-profile.component';
import { EditOperationRequestComponent } from './Components/OperationRequest/edit-operation-request/edit-operation-request.component';
import {AddAllergyComponent} from './Components/AllergyManagement/add-allergy/add-allergy.component';
import {SpecializationManagementComponent} from './Components/SpecializationManagement/specialization-management/specialization-management.component';
import {CreateSpecializationComponent} from './Components/SpecializationManagement/create-specialization/create-specialization.component';
import {AllergyManagementComponent} from './Components/AllergyManagement/allergy-management/allergy-management.component';
import {
  CreateAppointmentComponent
} from './Components/Appointment/createAppointment/create-appointment/create-appointment.component';
import{  PatientManagementAsDoctorComponent
} from './Components/PatientManagementAsADoctor/patientManagementAsDoctor/patient-management.component-as-doctor';
import {
  EditSpecializationComponent
} from './Components/SpecializationManagement/edit-specialization/edit-specialization.component';

export const routes: Routes = [
  {
    path: '',
    component: HomeComponent,
    title: 'Home',
  },

  {
    path: 'admin',
    canActivate: [AuthGuard],
    data: { roles: appSettings.userRoles.admin },
    children: [
      { path: '', component: AdminMenuComponent, title: 'AdminHome' },
      { path: 'operationTypeManagement', component: OperationTypeManagementComponent, title: 'OperationTypeManagement' },
      { path: 'operationTypeManagement/add', component: AddOperationTypeComponent, title: 'AddOperationType' },
      { path: 'operationTypeManagement/edit', component: EditOperationTypeComponent, title: 'EditOperationType' },
      { path: 'operationTypeManagement/view', component: ViewOperationTypeComponent, title: 'ViewOperationType' },
      { path: 'staff', component: StaffManagementComponent, title: 'StaffManagement' },
      { path: 'staff/add', component: AddStaffProfileComponent, title: 'AddStaffProfile' },
      { path: 'staff/edit', component: EditStaffProfileComponent, title: 'EditStaffProfile' },
      { path: 'patient', component: PatientManagementComponent, title: 'PatientManagement' },
      { path: 'patient/register', component: RegisterPatientProfileComponent, title: 'RegisterPatientProfile' },
      { path: 'patient/edit', component: EditPatientProfileComponent, title: 'EditPatientProfile' },
      { path: 'allergyManagement', component: AllergyManagementComponent, title: 'AllergyManagement' },
      { path: 'allergyManagement/add', component: AddAllergyComponent, title: 'AddAllergy' },
      { path: 'specialization', component: SpecializationManagementComponent, title: 'SpecializationManagement' },
      { path: 'specialization/add', component: CreateSpecializationComponent, title: 'CreateSpecialization' },
      { path: 'specialization/edit', component: EditSpecializationComponent, title: 'EditSpecialization' },
    ],
  },

  {
    path: 'staff',
    canActivate: [AuthGuard],
    data: { roles: appSettings.userRoles.staff},
    children: [
      { path: '', component: StaffHomeComponent, title: 'StaffHome' },
      { path: 'operationRequests', component: ListOperationRequestComponent, title: 'OperationRequests' },
      { path: 'operationRequest/add', component: AddOperationRequestComponent, title: 'AddOperationRequest' },
      { path: 'operationRequest/edit', component: EditOperationRequestComponent, title: 'EditOperationRequest' },
    ],
  },

  {
    path: 'patient',
    canActivate: [AuthGuard],
    data: { roles: appSettings.userRoles.patient },
    children: [
      { path: '', component: PatientHomeComponent, title: 'PatientDashboard' },
      { path: 'profile', component: PatientProfileComponent, title: 'PatientProfile' },
      { path: 'updateAccount', component: UpdatePatientAccoutComponent, title: 'UpdatePatientAccount' },
      { path: 'checkUserToDelete', component: CheckUserToDeleteComponent, title: 'CheckUserToDelete' },
      { path: 'deleteAccount', component: DeletePatientAccoutComponent, title: 'DeletePatientAccount' },
    ],
  },

  {
    path: '3d',
    canActivate: [AuthGuard],
    data: { roles: appSettings.userRoles.threeD },
    component: Connect3dComponent,
    title: '3D Module',
  },

  {
    path: 'unregistered',
    component: RegisterPatientProfileComponent,
    title: 'RegisterPatientProfile',
  }
];
