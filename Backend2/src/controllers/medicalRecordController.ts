import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";


@Service()
export default class MedicalRecordController implements IMedicalRecordController{
    
    constructor(
        @Inject(config.controllers.medicalRecord.name) private medicalRecordInstance: IMedicalRecordService
    ) {}

    
}