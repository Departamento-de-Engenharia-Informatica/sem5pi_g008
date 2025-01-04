export interface DisplayMedicalRecordConditionDTO{
  domainId?: string;
  conditionCode: string;
  conditionDesignation: string;
  doctorName: string;
  doctorLicenseNumber?: number;
  comment: string;
}
