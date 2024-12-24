import IMedicalConditionDTO from "./IMedicalConditionDTO";

export default interface IMedicalRecordConditionDTO{
  domainId?: string;
  conditionId: string;
  conditionDesignation?: string;
  medicalRecordId: string;
  doctorId?: string;
  doctorName?: string;
  doctorLicenseNumber?: number;
  comment: string;
}
